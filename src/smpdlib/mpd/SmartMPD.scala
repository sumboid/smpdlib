package smpd

case class SmartMPD(host: String, port: Int) {
  private[this] val mpd = MPD(host, port)
  private[this] var queue: List[Command] = Nil

  connect

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
        if (attemptNumber == 0) return x
        reconnect
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

  def wresponse(cer: => Unit = {},
                eer: => Unit = {},
                er: => Unit = {},
                ur: => Unit = {},
                r: => Unit = {},
                attemptNumber: Int = 3): Unit = response() match {
    case x: ConnectionErrorResponse => cer
    case x: ExternalErrorResponse => eer
    case x: ErrorResponse => er
    case x: UnknownResponse => ur
    case x: Response => r
  }

  def connect = mpd.connect
  def disconnect = mpd.disconnect
  def reconnect = mpd.reconnect
}
