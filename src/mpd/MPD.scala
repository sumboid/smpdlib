package smpd

import java.net._
import java.io._
import scala.io._

package mpdlow {

  trait LowLevelResponse[+A]

  case class Ok[+A](x: A) extends LowLevelResponse[A]
  case class Ack[+A] extends LowLevelResponse[A]
  case class MPDVersion[+A] extends LowLevelResponse[A]

  case object ExternalError extends LowLevelResponse[Nothing]
  case object InternalError extends LowLevelResponse[Nothing]
  case object ConnectionError extends LowLevelResponse[Nothing]
  case object UnknownHostError extends ConnectionError
  case object IOError extends ConnectionError

  case class MPDSocket(host: String, port: Int) {
    var socket: Socket = null
    var input:  BufferedReader = null
    var output: PrintStream = null

    var queue = 0

    def send(q: String) = {
      output.println(q)
      output.flush
      queue += 1
    }

    def recv = {
      if (queue == 0) ExternalError

      def _recv = {
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

      queue -= 1
      _recv
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
        case _ => InternalError
      }
    }

    def reconnect = disconnect; connect
  }
}

import mpdlow._

class MPD(host: String, port: Int) {
  val socket = MPDSocket(host, port)
  var queue: List[Response] = Nil

  def send(q: Command) = {
    checkConnectionState
    socket.send(q.raw)
    queue :+ q.response
  }

  def checkConnectionState = {
    socket.send("ping")
    socket.recv match {
      case ConnectionError => ConnectionError
      case _ => Ok
    }
  }

  def response = {
    if (queue == Nil) ExternalError

    _response match {

    }
  }
}

object MPD {
  def apply(host: String, port: Int) = new MPD(host, port)
}
