package smpd

trait Command {
  def response: Response
  def raw: String
}