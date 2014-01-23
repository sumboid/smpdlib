package smpd

case class Track(id:     Int,
                 pos:    Int,
                 track:  (Int, Int),
                 artist: String,
                 title:  String,
                 album:  String,
                 genre:  String,
                 time:   Int,
                 file:   String)

object Track {
  def create(raw: List[String]) = {
    var id = -1
    var pos = -1
    var track = -1 -> -1
    var artist = ""
    var title = ""
    var album = ""
    var genre = ""
    var time = -1
    var file = ""

    raw foreach { line =>
      val sline = line.split(": ", 2)
      val (keyword, value): (String, String) = (sline(0), sline(1))
      keyword match {
        case "Id" => id = value.toInt
        case "Pos" => pos = value.toInt
        case "Track" => value.split("/", 2) match {
          case Array(x) => track = x.toInt -> -1
          case Array(x, y) => track = x.toInt -> y.toInt
          case _ => {}
        }
        case "Artist" => artist = value
        case "Title" => title = value
        case "Album" => album = value
        case "Time" => time = value.toInt
        case "file" => file = value
        case _ => {}
      }
    }

    new Track(id, pos, track, artist, title, album, genre, time, file)
  }
}