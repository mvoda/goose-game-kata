package mvoda.goosegame

sealed trait GameError extends Throwable {
  def message: String
}

case class PlayerAlreadyExistsError(player: Player) extends GameError {
  override def message: String = s"$player: already existing player"
}

case class PlayerDoesNotExistsError(player: Player) extends GameError {
  override def message: String = s"$player: does not exist"
}
