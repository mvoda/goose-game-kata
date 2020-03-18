package mvoda.goosegame.events

sealed trait GameError extends Throwable with GameMessage

case class InvalidMove(first: Int, second: Int) extends GameError {
  override val msg: String = s"invalid dice values: $first, $second"
}

case class InvalidCommand(string: String) extends GameError {
  override val msg: String = s"invalid command: $string"
}
