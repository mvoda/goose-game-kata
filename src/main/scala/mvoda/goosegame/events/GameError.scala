package mvoda.goosegame.events

import mvoda.goosegame.game.Player

sealed trait GameError extends Throwable with GameMessage

case class InvalidMove(first: Int, second: Int) extends GameError {
  override val msg: String = s"invalid dice values: $first, $second"
}

case class InvalidCommand(string: String) extends GameError {
  override val msg: String = s"invalid command: $string"
}

case class PlayerAlreadyExists(player: Player) extends GameError {
  override val msg: String = s"$player: already existing player."
}

case class PlayerDoesNotExist(player: Player) extends GameError {
  override val msg: String = s"$player: does not exist."
}

case class GameEnded(player: Player) extends GameError {
  override val msg: String = s"Game has already ended. $player won!!"
}
