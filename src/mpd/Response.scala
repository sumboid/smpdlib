package smpd

trait Response {
  def parse(raw: List[String]): Response
}

case class ErrorResponse(error: String = "") extends Response {
  def parse(raw: List[String]) = new ErrorResponse(raw.mkString("\n"))
}