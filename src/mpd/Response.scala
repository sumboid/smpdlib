package smpd

trait Response {
  def parse(raw: List[String]): Response
}

case class ErrorResponse extends Response {
  def parse(raw: List[String]) = new ErrorResponse
}