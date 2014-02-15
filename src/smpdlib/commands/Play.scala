package smpd

case class Play(pos: Int = 0) extends Command {
  val command = "play"
  def raw = command :: pos :: Nil mkString (" ") 

  def response = PlayResponse()
}

case class PlayResponse extends Response {
  def parse(raw: List[String]) = new PlayResponse
}