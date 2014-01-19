package mpd

import java.net._
import java.io._
import scala.io._

class MPD(host: String, port: Int) {
  val _host = host
  val _port = port
  var socket: Socket = null
  var input: BufferedReader = null
  var output: PrintStream = null

  connect

  def get(q: String) {
    output.println(q)
    output.flush
    response.foreach(println)
  }

  def response: List[String] = {
    var lines = List[String]()
    while(true) {
      val line = input.readLine
      if(line == null) { println("Error while reading from socket"); return Nil }
      line.split(" ")(0) match {
        case "OK"  => return lines
        case "ACK" => return Nil
        case _ => lines ::= line
      }
    }
    lines
  }

  def connect {
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
      case _: Throwable => println("Something wrong FUCK")
    }

    { response }
  }
}

object MPD {
  def apply(host: String, port: Int) = new MPD(host, port)
}