package mvoda.goosegame.rules

import mvoda.goosegame.events.{ ExistingPlayers, GameError, PlayerAlreadyExists }
import mvoda.goosegame.game.{ Game, GameUpdate, Player }

object AddRules {

  /**
    * Adds a player to the game
    * @param game game state
    * @param player player to be added
    * @return new game state and log, or error if the player already exists
    */
  def addPlayer(game: Game, player: Player): Either[GameError, GameUpdate] =
    if (game.playerPositions.contains(player)) {
      Left(PlayerAlreadyExists(player))
    } else {
      val newGame = game.copy(playerPositions = game.playerPositions + (player -> 0))
      val update  = ExistingPlayers(newGame.playerPositions.keySet)
      Right(GameUpdate(newGame, Seq(update)))
    }
}
