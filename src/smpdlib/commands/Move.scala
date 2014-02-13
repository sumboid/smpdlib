package smpd

case class Move(from: Int, to: Int) extends Command {
  val command = "move"
  def raw = command :: from :: to :: Nil mkString (" ")

  def response = MoveResponse()
}

case class MoveMultiple(from: (Int, Int), to: Int) extends Command {
  val command = "move"
  def raw = command :: from._1 + ":" + from._2 :: to :: Nil mkString (" ")

  def response = MoveResponse()
}

case class MoveResponse extends Response {
  def parse(raw: List[String]) = new MoveResponse
}