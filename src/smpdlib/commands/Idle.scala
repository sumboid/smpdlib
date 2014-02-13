package smpd

package sub {
  abstract class Subsystem(name: String = "") {
    override def toString = name
  }
  case class Database extends Subsystem("database")
  case class Update extends Subsystem("update")
  case class StoredPlaylist extends Subsystem("stored_playlist")
  case class Playlist extends Subsystem("playlist")
  case class Player extends Subsystem("player")
  case class Mixer extends Subsystem("mixer")
  case class Output extends Subsystem("output")
  case class Sticker extends Subsystem("sticker")
  case class Subscription extends Subsystem("subscription")
  case class Message extends Subsystem("message")
  case class All extends Subsystem

  object Subsystem {
    def parse(raw: List[String]) = {
      var subsystems: List[Subsystem] = Nil
      raw foreach {
        _.split(": ")(1) match {
          case "database" => subsystems ::= Database()
          case "update" => subsystems ::= Update()
          case "stored_playlist" => subsystems ::= StoredPlaylist()
          case "playlist" => subsystems ::= Playlist()
          case "player" => subsystems ::= Player()
          case "mixer" => subsystems ::= Mixer()
          case "output" => subsystems ::= Output()
          case "sticker" => subsystems ::= Sticker()
          case "subscription" => subsystems ::= Subscription()
          case "message" => subsystems ::= Message()
          case _ => {}
        }
      }
      subsystems
    }
  }
}

import sub._

case class Idle(subsystems: List[Subsystem] = All() :: Nil) extends Command {
  val command = "idle"
  def raw = (command :: subsystems).mkString(" ")
  def response = IdleResponse()
}

case class IdleResponse(subsystems: List[Subsystem] = Nil) extends Response {
  def parse(raw: List[String]) = new IdleResponse(Subsystem.parse(raw))
}