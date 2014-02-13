package smpd

case class AddId(uri: String, pos: String = "") extends Command {
  val command = "addid"
  def raw = command :: "\"" + uri + "\"" :: pos :: Nil mkString (" ") // XXX

  def response = AddResponse()
}

case class AddIdResponse(id: Int) extends Response {
  def parse(raw: List[String]) = new AddIdResponse(raw(0).split(": ")(1).toInt)
}