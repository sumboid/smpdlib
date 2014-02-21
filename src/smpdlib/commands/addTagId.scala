package smpd

case class AddTagId(id: Int, tag: String, value: String) extends Command {
  val command = "addtagid"
  def raw = command :: id :: tag :: value :: Nil mkString (" ")

  def response = AddTagIdResponse()
}

case class AddTagIdResponse extends Response {
  def parse(raw: List[String]) = new AddTagIdResponse
}