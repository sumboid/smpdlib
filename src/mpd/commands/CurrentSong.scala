package mpd

case class CurrentSong extends Command {
  val raw = "currentsong"

  def response = CurrentSongResponse()
}

case class CurrentSongResponse(track: Track = null) extends Response {
  def parse(raw: List[String]) = new CurrentSongResponse(Track.create(raw))
}