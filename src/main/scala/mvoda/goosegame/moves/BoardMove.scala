package mvoda.goosegame.moves

import mvoda.goosegame.{ BoardSpace, GameMessage, Player }

sealed trait BoardMove extends GameMessage {
  def player: Player
  def start: BoardSpace
  def end: BoardSpace
}

case class Return(player: Player, start: BoardSpace, end: BoardSpace) extends BoardMove {
  override def msg: String = s"On $start there is $player, who returns to $end."
}

case class Advance(player: Player, start: BoardSpace, end: BoardSpace) extends BoardMove {
  override def msg: String = s"$player moves from $start to $end."
}

case class Overshot(player: Player, start: BoardSpace, end: BoardSpace, overshotSpaces: Int) extends BoardMove {
  override def msg: String = s"$player moves from $start to $end."
}

case class Bounce(player: Player, start: BoardSpace, end: BoardSpace) extends BoardMove {
  override def msg: String = s"$player bounces! $player returns to $end."
}

case class Jump(player: Player, start: BoardSpace, end: BoardSpace) extends BoardMove {
  override def msg: String = s"$player jumps to $end."
}

case class ExtraMove(move: BoardMove) extends BoardMove with GameMessage {
  val player: Player       = move.player
  val start: BoardSpace    = move.start
  val end: BoardSpace      = move.end
  override def msg: String = s"$player moves again and goes to $end."
}
