package mvoda.goosegame.game

case class Game(playerPositions: Map[Player, Int] = Map(), board: Board = Board(), winner: Option[Player] = None)