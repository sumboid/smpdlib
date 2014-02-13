package smpd

case class MPD(host: String, port: Int) {
  private[this] val socket = MPDSocket(host, port)
  var version = ""

  def send(q: Command) = {
    socket.send(q.raw)
  }

  def response(q: Command) = socket.recv match {
    case Ok(x) => q.response.parse(x)
    case Ack(x) => ErrorResponse(x(0))
    case ConnectionError => ConnectionErrorResponse()
    case _ => UnknownResponse()
  }

  def connect = socket.connect match {
    case MPDVersion(x) => version = x(0)
    case _ => ConnectionError
  }

  def disconnect = socket.disconnect
  def reconnect = { disconnect; connect }
}