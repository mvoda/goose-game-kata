package mvoda.goosegame.rules

import mvoda.goosegame._

object AddRules {
  def addPlayer(game: Game, player: Player): GameUpdate =
    if (game.playerPositions.contains(player)) {
      val update = PlayerAlreadyExists(player)
      GameUpdate(game, Seq(update))
    } else {
      val newPlayerPositions = game.playerPositions + (player -> 0)
      val update             = ExistingPlayers(newPlayerPositions.keySet)
      GameUpdate(game.copy(playerPositions = newPlayerPositions), Seq(update))
    }
}
