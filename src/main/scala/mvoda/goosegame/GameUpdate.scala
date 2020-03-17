package mvoda.goosegame

import mvoda.goosegame.moves.BoardMove

case class GameUpdate(game: Game, log: Seq[BoardMove])
