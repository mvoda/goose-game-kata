package mvoda.goosegame

sealed trait BoardSpace

case class Bridge(toPosition: Int) extends BoardSpace
case object Goose extends BoardSpace
case object EmptySpace extends BoardSpace
