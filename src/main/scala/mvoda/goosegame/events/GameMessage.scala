package mvoda.goosegame.events

import mvoda.goosegame.game.Player

trait GameMessage {
  def msg: String
}

case class PlayerAlreadyExists(player: Player) extends GameMessage {
  override val msg: String = s"$player: already existing player."
}

case class PlayerDoesNotExist(player: Player) extends GameMessage {
  override val msg: String = s"$player: does not exist."
}

case class ExistingPlayers(playerSet: Set[Player]) extends GameMessage {
  override val msg: String = s"players: ${playerSet.map(_.name).mkString(", ")}."
}

case class PlayerRolls(player: Player, firstDice: Int, secondDice: Int) extends GameMessage {
  override val msg: String = s"$player rolls $firstDice, $secondDice."
}
