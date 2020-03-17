package mvoda.goosegame

sealed trait PlayerUpdate extends LogMessage

case class PlayerAlreadyExists(player: Player) extends PlayerUpdate {
  override def log: String = s"$player: already existing player"
}

case class PlayerDoesNotExists(player: Player) extends PlayerUpdate {
  override def log: String = s"$player: does not exist"
}

case class ExistingPlayers(playerSet: Set[Player]) extends LogMessage {
  override def log: String = s"players: ${playerSet.map(_.name).mkString(", ")}"
}
