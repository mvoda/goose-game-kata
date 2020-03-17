package mvoda.goosegame

sealed trait GameError extends Throwable with LogMessage {
  def message: String
}

case class PlayerAlreadyExistsError(player: Player) extends GameError {
  override def message: String = s"$player: already existing player"
  override def log: String     = message
}

case class PlayerDoesNotExistsError(player: Player) extends GameError {
  override def message: String = s"$player: does not exist"
  override def log: String     = message
}
