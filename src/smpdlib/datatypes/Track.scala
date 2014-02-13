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
  def specialToInt(number: String) = try {
    number.toInt
  } catch {
    case _: Throwable => -1
  }

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
        case "Id" => id = specialToInt(value)
        case "Pos" => pos = specialToInt(value)
        case "Track" => value.split("/", 2) match {
          case Array(x) => track = specialToInt(x) -> -1
          case Array(x, y) => track = specialToInt(x) -> specialToInt(y)
          case _ => {}
        }
        case "Artist" => artist = value
        case "Title" => title = value
        case "Album" => album = value
        case "Time" => time = specialToInt(value)
        case "file" => file = value
        case _ => {}
      }
    }

    new Track(id, pos, track, artist, title, album, genre, time, file)
  }
}