package mvoda.goosegame.rules

import mvoda.goosegame.commands.{ Add, Command, Move }
import mvoda.goosegame.events.GameError
import mvoda.goosegame.game.{ Game, GameUpdate }

object GameRules {
  def applyCommand(game: Game, command: Command): Either[GameError, GameUpdate] =
    command match {
      case Add(player) => AddRules.addPlayer(game, player)
      case move: Move  => MoveRules.movePlayer(game, move)
    }
}
