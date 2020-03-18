package mvoda.goosegame.rules

import mvoda.goosegame.events.{ ExistingPlayers, GameError, PlayerAlreadyExists }
import mvoda.goosegame.game.{ Game, GameUpdate, Player }

object AddRules {
  def addPlayer(game: Game, player: Player): Either[GameError, GameUpdate] =
    if (game.playerPositions.contains(player)) {
      Left(PlayerAlreadyExists(player))
    } else {
      val newGame = game.copy(playerPositions = game.playerPositions + (player -> 0))
      val update  = ExistingPlayers(newGame.playerPositions.keySet)
      Right(GameUpdate(newGame, Seq(update)))
    }
}
