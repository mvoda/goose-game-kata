package mvoda.goosegame.rules

import mvoda.goosegame
import mvoda.goosegame.commands.Move
import mvoda.goosegame.events._
import mvoda.goosegame.game.{ GameUpdate, _ }

import scala.annotation.tailrec

object MoveRules {

  def movePlayer(game: Game, move: Move): GameUpdate = {
    if (!game.playerPositions.contains(move.player)) {
      val message = PlayerDoesNotExist(move.player)
      GameUpdate(game, Seq(message))
    } else {
      val moveUpdate  = process(game, move.player, move.spaces)
      val rollMessage = PlayerRolls(move.player, move.firstDice, move.secondDice)
      moveUpdate.copy(log = rollMessage +: moveUpdate.log)
    }
  }

  def process(game: Game, player: Player, spaces: Int): GameUpdate = {
    val startPosition   = game.playerPositions(player)
    val firstPlayerMove = computePlayerMove(game.board, player, startPosition, spaces)
    val gameUpdate      = goosegame.game.GameUpdate(game, Seq())
    processMoveChain(gameUpdate, firstPlayerMove, spaces)
  }

  @tailrec
  def processMoveChain(currentUpdate: GameUpdate, boardMove: BoardMove, moveSpaces: Int): GameUpdate = {
    val gameUpdate = processMove(currentUpdate, boardMove)
    val board      = gameUpdate.game.board
    boardMove.end match {
      case Bridge(_, toPosition) =>
        val nextMove = Jump(boardMove.player, boardMove.end, gameUpdate.game.board.get(toPosition))
        processMoveChain(gameUpdate, nextMove, moveSpaces)

      case Goose(_) =>
        val computedMove = computePlayerMove(gameUpdate.game.board, boardMove.player, boardMove.end.position, moveSpaces)
        if (causesInfiniteLoop(boardMove.end.position, moveSpaces, board.endPosition)) gameUpdate
        else processMoveChain(gameUpdate, ExtraMove(computedMove), moveSpaces)

      case EmptySpace(board.endPosition) =>
        extractMove(boardMove) match {
          case Overshot(player, _, end, overshotSpaces) =>
            val nextMove = Bounce(player, end, board.get(end.position - overshotSpaces))
            processMoveChain(gameUpdate, nextMove, moveSpaces)
          case _ => gameUpdate
        }

      case _ => gameUpdate
    }
  }

  def processMove(currentUpdate: GameUpdate, boardMove: BoardMove): GameUpdate = {
    val maybePrankedPlayerMove = prankedPlayerMove(currentUpdate.game.playerPositions, boardMove)
    val update                 = updateGame(currentUpdate, boardMove)
    maybePrankedPlayerMove.fold(update)(prankedPlayerMove => updateGame(update, prankedPlayerMove))
  }

  def updateGame(previousUpdate: GameUpdate, boardMove: BoardMove): GameUpdate = {
    if (previousUpdate.game.winner.isDefined) {
      previousUpdate
    } else {
      val newPositions = previousUpdate.game.playerPositions + (boardMove.player -> boardMove.end.position)
      val winner       = determineWinner(previousUpdate.game.board, boardMove)
      val updatedGame  = previousUpdate.game.copy(playerPositions = newPositions, winner = winner)
      val updatedLog   = previousUpdate.log :+ boardMove
      GameUpdate(updatedGame, updatedLog)
    }
  }

  def determineWinner(board: Board, move: BoardMove): Option[Player] = move match {
    case _: Overshot | ExtraMove(_: Overshot)        => None
    case _ if move.end.position == board.endPosition => Some(move.player)
    case _                                           => None
  }

  def prankedPlayerMove(playerPositions: Map[Player, Int], boardMove: BoardMove): Option[BoardMove] = {
    val maybePrankedPlayer = playerPositions.find { case (_, position) => position == boardMove.end.position && position != 0 }.map(_._1)
    maybePrankedPlayer.map(prankedPlayer => Return(prankedPlayer, boardMove.end, boardMove.start))
  }

  def computePlayerMove(board: Board, player: Player, startPosition: Int, moveSpaces: Int): BoardMove = {
    val nextPosition = startPosition + moveSpaces
    if (nextPosition <= board.endPosition) {
      Advance(player, board.get(startPosition), board.get(nextPosition))
    } else {
      val overshotSpaces = nextPosition - board.endPosition
      Overshot(player, board.get(startPosition), board.get(board.endPosition), overshotSpaces)
    }
  }

  private def causesInfiniteLoop(startPosition: Int, moveSpaces: Int, endPosition: Int): Boolean = {
    val nextPosition   = startPosition + moveSpaces
    val overshotSpaces = math.abs(nextPosition - endPosition)
    val finalPosition  = endPosition - overshotSpaces
    finalPosition == startPosition
  }

  private def extractMove(boardMove: BoardMove): BoardMove = boardMove match {
    case ExtraMove(move) => move
    case move            => move
  }

}
