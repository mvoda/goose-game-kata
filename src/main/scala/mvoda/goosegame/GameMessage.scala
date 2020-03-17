package mvoda.goosegame

trait GameMessage {
  def message: String
}

case class PlayerAlreadyExists(player: Player) extends GameMessage {
  override def message: String = s"$player: already existing player"
}

case class PlayerDoesNotExist(player: Player) extends GameMessage {
  override def message: String = s"$player: does not exist"
}

case class ExistingPlayers(playerSet: Set[Player]) extends GameMessage {
  override def message: String = s"players: ${playerSet.map(_.name).mkString(", ")}"
}
