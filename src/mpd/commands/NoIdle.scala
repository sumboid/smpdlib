package smpd

case class NoIdle extends Command {
  val raw = "noidle"
  def response = NoIdleResponse()
}

case class NoIdleResponse extends Response {
  def parse(raw: List[String]) = new IdleResponse()
}