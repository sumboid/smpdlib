package smpd

import java.net._
import java.io._
import scala.io._

package mpdlow {

  trait LowLevelResponse[+A]

  case class Ok[+A](x: A) extends LowLevelResponse[A]
  case class Ack[+A](x: A) extends LowLevelResponse[A]
  case class MPDVersion[+A](x: A) extends LowLevelResponse[A]

  case object ExternalError extends LowLevelResponse[Nothing]
  case object InternalError extends LowLevelResponse[Nothing]
  case object ConnectionError extends LowLevelResponse[Nothing]
  case object UnknownHostError extends ConnectionError
  case object IOError extends ConnectionError

  case class MPDSocket(host: String, port: Int) {
    var socket: Socket = null
    var input:  BufferedReader = null
    var output: PrintStream = null

    def send(q: String) = {
      output.println(q)
      output.flush
    }

    def recv = {
      var lines = List[String]()
      while(true) {
        var line: String = input.readLine
        if(line == null) { return ConnectionError }

        line.split(" ", 2) match {
          case Array("OK") => return Ok(lines)
          case Array("OK", message) => message.split(" ", 2) match {
            case Array("MPD", vesrion) => return MPDVersion(version)
            case _ => return Ok(lines)
          }
          case Array("ACK", message) => return Ack(message)
          case _ => lines ::= line
        }
      }
      Ok(lines)
    }

    def disconnect = if(socket != null) {
      socket.close
      input.close
      output.close

      socket = null
      input  = null
      output = null
    }

    def connect = {
      try {
        socket = new Socket(InetAddress.getByName(host), port)
        input  = new BufferedReader(new InputStreamReader(socket.getInputStream))
        output = new PrintStream(socket.getOutputStream)
      } catch {
        case e: UnknownHostException => UnknownHostError
        case e: IOException => IOError
        case _: Throwable => ConnectionError
      }

      recv match {
        case x: MPDVersion => x
        case _             => InternalError
      }
    }

    def reconnect = disconnect; connect
  }
}

import mpdlow._

case class MPD(host: String, port: Int) {
  private[this] val socket = MPDSocket(host, port)
  var version = ""

  def send(q: Command) = {
    checkConnectionState
    socket.send(q.raw)
  }

  def response(q: Command) = socket.recv match {
    case Ok(x) => q.response.parse(x)
    case Ack(x) => ErrorResponse(x)
    case ConnectionError => ConnectionErrorResponse()
    case _ => UnknownResponse()
  }

  def connect = socket.connect match {
    case MPDVersion(x) => version = x
    case _ => ConnectionError
  }

  def disconnect = socket.disconnect
  def reconnect = disconnect; connect
}

case class SmartMPD(host: String, port: Int) {
  private[this] val mpd = MPD(host, port)
  private[this] var queue: List[Command] = Nil

  def send(q: Command) = {
    mpd.send(q)
    queue :+ q
  }

  def response(attemptNumber: Int = 3): Response = {
    if (queue.head == Nil) ExternalErrorResponse()

    val query = queue.head
    queue = queue.tail

    mpd.response(query) match {
      case x: ConnectionErrorResponse => {
        if (attemptNumber == 0) x
        var newQueue: List[Command] = Nil
        queue.reverse foreach (newQueue ::= _)
        newQueue ::= query

        queue = Nil
        newQueue foreach send
        response(attemptNumber - 1)
      }

      case x: Response => x
    }
  }

  def connect = mpd.connect
  def disconnect = mpd.disconnect
  def reconnect = mpd.reconnect
}
