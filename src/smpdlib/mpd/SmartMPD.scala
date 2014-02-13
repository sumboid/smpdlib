package smpd

case class SmartMPD(host: String, port: Int) {
  private[this] val mpd = MPD(host, port)
  private[this] var queue: List[Command] = Nil

  def send(q: Command) = {
    mpd.send(q)
    queue = queue :+ q
  }

  def response(attemptNumber: Int = 3): Response = {
    if (queue.isEmpty) return ExternalErrorResponse()

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
