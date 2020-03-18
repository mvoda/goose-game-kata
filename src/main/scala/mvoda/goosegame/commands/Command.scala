package mvoda.goosegame.commands

import mvoda.goosegame.Player

sealed trait Command

case class Add(player: Player) extends Command

case class Move(player: Player, firstDice: Int, secondDice: Int) extends Command {
  val spaces: Int = firstDice + secondDice
}
