package mvoda.goosegame.commands

import mvoda.goosegame.Player

sealed trait Command

case class Add(player: Player) extends Command

case class Move(player: Player, spaces: Int) extends Command
