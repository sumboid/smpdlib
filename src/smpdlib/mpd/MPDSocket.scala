package smpd

import java.net._
import java.io._
import scala.io._

trait LowLevelResponse[+A]

case class Ok[+A](x: A) extends LowLevelResponse[A]
case class Ack[+A](x: A) extends LowLevelResponse[A]
case class MPDVersion[+A](x: A) extends LowLevelResponse[A]

case object ExternalError extends LowLevelResponse[Nothing]
case object InternalError extends LowLevelResponse[Nothing]
case object ConnectionError extends LowLevelResponse[Nothing]
case object UnknownHostError extends LowLevelResponse[Nothing]
case object IOError extends LowLevelResponse[Nothing]

case class MPDSocket(host: String, port: Int) {
  var socket: Socket = null
  var input:  BufferedReader = null
  var output: PrintStream = null

  def send(q: String) = {
    output.println(q)
    output.flush
  }

  def recv: LowLevelResponse[List[String]] = {
    var lines = List[String]()
    while(true) {
      var line: String = input.readLine
      if(line == null) { return ConnectionError }

      line.split(" ", 2) match {
        case Array("OK") => return Ok(lines)
        case Array("OK", message) => message.split(" ", 2) match {
          case Array("MPD", version) => return MPDVersion(version :: Nil)
          case _ => return Ok(lines)
        }
        case Array("ACK", message) => return Ack(message :: Nil)
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
      case x:MPDVersion[List[String]] => x
      case _                          => InternalError
    }
  }

  def reconnect = disconnect; connect
}
