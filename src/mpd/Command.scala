package mpd

trait Command {
  def response: Response
  def raw: String
}