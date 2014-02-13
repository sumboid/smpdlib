package smpd

case class Add(uri: String) extends Command {
  val command = "add"
  def raw = command :: "\"" + uri + "\"" :: Nil mkString (" ") // XXX

  def response = AddResponse()
}

case class AddResponse extends Response {
  def parse(raw: List[String]) = new AddResponse
}