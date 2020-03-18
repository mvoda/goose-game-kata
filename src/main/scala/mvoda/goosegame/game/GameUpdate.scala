package mvoda.goosegame.game

import mvoda.goosegame.events.GameMessage

case class GameUpdate(game: Game, log: Seq[GameMessage])
