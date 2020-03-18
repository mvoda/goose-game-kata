package mvoda.goosegame.game

sealed trait BoardSpace {
  def position: Int
}

case class Start(position: Int) extends BoardSpace {
  override def toString: String = "Start"
}

case class Bridge(position: Int, toPosition: Int) extends BoardSpace {
  override def toString: String = "The Bridge"
}

case class Goose(position: Int) extends BoardSpace {
  override def toString: String = s"$position, The Goose"
}

case class EmptySpace(position: Int) extends BoardSpace {
  override def toString: String = s"$position"
}
