package mvoda.goosegame

case class Player(name: String) extends AnyVal {
  override def toString: String = name
}
