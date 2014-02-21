package smpd

package stat {
  trait StateStatus
  case object Play extends StateStatus
  case object Stop extends StateStatus
  case object Pause extends StateStatus

  trait Stat
  case class State(s: StateStatus) extends Stat

  object Stat {
    def parse(raw: List[String]) = {
      var stats: List[Stat] = Nil
      raw foreach {
        _.split(": ") match {
          case Array("state", x) => x match {
            case "play"  => stats ::= State(Play)
            case "stop"  => stats ::= State(Stop)
            case "pause" => stats ::= State(Pause)
            case _       => {}
          }
          case _ => {}
        }
      }
      stats
    }
  }
}

import stat._

case class Status extends Command {
  val raw = "status"
  def response = StatusResponse()
}

case class StatusResponse(stats: List[Stat] = Nil) extends Response {
  def parse(raw: List[String]) = new StatusResponse(Stat.parse(raw))
}