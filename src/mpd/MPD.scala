package mpd

import java.net._
import java.io._
import scala.io._

trait LowLevelResponse[+A]
case class Ok[+A](x: A) extends LowLevelResponse[A]
case object Ack extends LowLevelResponse[Nothing]
case object ConnectionError extends LowLevelResponse[Nothing]

class MPD(host: String, port: Int) {
  val _host = host
  val _port = port
  var socket: Socket = null
  var input: BufferedReader = null
  var output: PrintStream = null

  connect

  def get(q: Command): Response = {
    output.println(q.raw)
    output.flush

    response match {
      case ConnectionError => connect; get(q)
      case Ack => ErrorResponse()
      case Ok(x) => q.response.parse(x)
    }
  }

  def response: LowLevelResponse[List[String]] = {
    var lines = List[String]()
    while(true) {

      var line: String = input.readLine
      if(line == null) { return ConnectionError }
      line.split(" ")(0) match {
        case "OK"  => return Ok(lines)
        case "ACK" => return Ack
        case _ => lines ::= line
      }
    }
    Ok(lines)
  }

  def connect = {
    if(socket != null) {
      socket.close
      input.close
      output.close
    }
    
    try {
      socket = new Socket(InetAddress.getByName(_host), _port)
      input = new BufferedReader(new InputStreamReader(socket.getInputStream))
      output = new PrintStream(socket.getOutputStream)
    } catch {
      case e: UnknownHostException => println("Unknown host")
      case e: IOException => println("No I/O")
      case _: Throwable => println("Something wrong")
    }

    { response }
  }
}

object MPD {
  def apply(host: String, port: Int) = new MPD(host, port)
}