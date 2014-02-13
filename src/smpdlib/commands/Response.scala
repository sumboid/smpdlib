package smpd

trait Response {
  def parse(raw: List[String]): Response
}

case class ErrorResponse(error: String = "") extends Response {
  def parse(raw: List[String]) = new ErrorResponse(raw.mkString("\n"))
}

case class UnknownResponse extends Response {
  def parse(raw: List[String]) = new UnknownResponse
}

case class ConnectionErrorResponse extends Response {
  def parse(raw: List[String]) = new ConnectionErrorResponse
}

case class ExternalErrorResponse extends Response {
  def parse(raw: List[String]) = new ExternalErrorResponse
}
