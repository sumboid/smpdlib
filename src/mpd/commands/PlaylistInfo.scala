package mpd

class PlaylistInfo extends Command {
  val command = "playlistinfo"

  def raw = command
  def response = PlaylistInfoResponse()
}

case class PlaylistInfoResponse(playlist: List[Track] = Nil) extends Response {
  def splitTracks(tracks: List[String]): List[List[String]] = tracks match {
    case Nil => Nil
    case _ => {
      val end = tracks.find(line => line.split(": ")(0) == "file").get
      val index = tracks.indexOf(end) + 1
      val (head, tail) = tracks.splitAt(index)
      head :: splitTracks(tail)
    }
  }

  def parse(raw: List[String]) = {
    val rawTracks = splitTracks(raw)
    PlaylistInfoResponse(rawTracks map (Track.create(_)))
  }
}