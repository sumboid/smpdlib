package smpd

case class Update(uri: String = "") extends Command {
  val command = "update"
  def raw = command :: uri :: Nil mkString (" ")

  def response = UpdateResponse()
}

case class UpdateResponse(id: Int = 0) extends Response {
  def parse(raw: List[String]) = new UpdateResponse(raw(0).split(": ")(1).toInt)
}