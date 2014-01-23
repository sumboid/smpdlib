package smpd
import sub._

case class Idle(subsystems: List[Subsystem] = All() :: Nil) extends Command {
  val command = "idle"
  def raw = (command :: subsystems).mkString(" ")
  def response = IdleResponse()
}

case class IdleResponse(subsystems: List[Subsystem] = Nil) extends Response {
  def parse(raw: List[String]) = new IdleResponse(Subsystem.parse(raw))
}