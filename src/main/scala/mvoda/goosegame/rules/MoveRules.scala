package mvoda.goosegame.rules

import mvoda.goosegame.commands.Move
import mvoda.goosegame.events._
import mvoda.goosegame.game.{ GameUpdate, _ }

import scala.annotation.tailrec

object MoveRules {

  /**
    * Applies the move to the current game state
    * @param game game state
    * @param move move
    * @return new game state and log, or errors in case the player does not exist or the game has already finished.
    */
  def movePlayer(game: Game, move: Move): Either[GameError, GameUpdate] = {
    if (!game.playerPositions.contains(move.player)) {
      Left(PlayerDoesNotExist(move.player))
    } else if (game.winner.isDefined) {
      Left(GameEnded(game.winner.get))
    } else {
      val rollMessage     = PlayerRolls(move.player, move.firstDice, move.secondDice)
      val update          = process(game, move.player, move.spaces)
      val maybeWinMessage = update.game.winner.map(PlayerWins)
      val newLog          = (rollMessage +: update.log) ++ maybeWinMessage.toSeq
      Right(update.copy(log = newLog))
    }
  }

  /**
    * Computes the move the player should make and returns the ending state after processing the entire move chain.
    * @param game game state
    * @param player player making the move
    * @param moveSpaces number of spaces the player should move forward
    * @return new game state and log
    */
  def process(game: Game, player: Player, moveSpaces: Int): GameUpdate = {
    val startPosition   = game.playerPositions(player)
    val firstPlayerMove = computeForwardMove(game.board, player, startPosition, moveSpaces)
    val gameUpdate      = GameUpdate(game, Seq())
    processMoveChain(gameUpdate, firstPlayerMove, moveSpaces)
  }

  /**
    * Recursively processes a sequence of moves and updates the game state and log.
    * A sequence of moves occurs when moving a player triggers new moves:
    *  - a move to the Bridge triggers a Jump move
    *  - a move to the Goose triggers an extra move with the same number of spaces
    *  - an overshot move past the end of the board triggers a bounce
    *
    * @param currentUpdate current game state and log
    * @param boardMove move to be applied
    * @param moveSpaces number of spaces to move (in case of an extra move)
    * @return updated game state and log
    */
  @tailrec
  def processMoveChain(currentUpdate: GameUpdate, boardMove: BoardMove, moveSpaces: Int): GameUpdate = {
    val gameUpdate = movePlayerAndPrankedPlayer(currentUpdate, boardMove)
    val board      = gameUpdate.game.board
    boardMove.end match {
      case Bridge(_, toPosition) =>
        val nextMove = Jump(boardMove.player, boardMove.end, gameUpdate.game.board.get(toPosition))
        processMoveChain(gameUpdate, nextMove, moveSpaces)

      case Goose(_) if !causesInfiniteLoop(boardMove.end.position, moveSpaces, board.endPosition) =>
        val nextMove = computeForwardMove(gameUpdate.game.board, boardMove.player, boardMove.end.position, moveSpaces)
        processMoveChain(gameUpdate, ExtraMove(nextMove), moveSpaces)

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

  /**
    * Applies the [[BoardMove]] and the potential move of the pranked player and updates the game state and log.
    * @param currentUpdate current game state and log
    * @param boardMove move to be applied
    * @return next game state and log
    */
  def movePlayerAndPrankedPlayer(currentUpdate: GameUpdate, boardMove: BoardMove): GameUpdate = {
    val maybePrankedPlayerMove = findPrankedPlayerMove(currentUpdate.game.playerPositions, boardMove)
    val update                 = applyMoveOnBoard(currentUpdate, boardMove)
    maybePrankedPlayerMove.fold(update)(prankedPlayerMove => applyMoveOnBoard(update, prankedPlayerMove))
  }

  /**
    * Applies the [[BoardMove]], updating the current game state and log. Does nothing if the game already finished.
    * @param currentUpdate current game state and log
    * @param boardMove move to be applied
    * @return next game state and log.
    */
  def applyMoveOnBoard(currentUpdate: GameUpdate, boardMove: BoardMove): GameUpdate = {
    if (currentUpdate.game.winner.isDefined) {
      currentUpdate
    } else {
      val newPositions = currentUpdate.game.playerPositions + (boardMove.player -> boardMove.end.position)
      val winner       = determineWinner(currentUpdate.game.board, boardMove)
      val updatedGame  = currentUpdate.game.copy(playerPositions = newPositions, winner = winner)
      val updatedLog   = currentUpdate.log :+ boardMove
      GameUpdate(updatedGame, updatedLog)
    }
  }

  /**
    * Finds if a (pranked) player currently occupies the end position of this [[BoardMove]] and returns the move that the pranked player should make to swap positions
    * @param playerPositions current player positions on the board
    * @param boardMove the move of the player
    * @return a [[Return]] move if the end position of this [[BoardMove]] is occupied, empty otherwise
    */
  def findPrankedPlayerMove(playerPositions: Map[Player, Int], boardMove: BoardMove): Option[Return] = {
    val maybePrankedPlayer = playerPositions.find { case (_, position) => position == boardMove.end.position && position != 0 }.map(_._1)
    maybePrankedPlayer.map(prankedPlayer => Return(prankedPlayer, boardMove.end, boardMove.start))
  }

  /**
    * Computes how far the player should move forward after throwing the dice
    * @param board the game board
    * @param player the player to be moved
    * @param startPosition player's starting position
    * @param moveSpaces how many spaces to move forward
    * @return a move indicating the start/end positions:
    *         - [[Advance]] if the move stopped before or on the last space on the board
    *         - [[Overshot]] if the move would extend past the end of the board, with overshotSpaces indicating how many move spaces were not used
    */
  def computeForwardMove(board: Board, player: Player, startPosition: Int, moveSpaces: Int): BoardMove = {
    val nextPosition = startPosition + moveSpaces
    if (nextPosition <= board.endPosition) {
      Advance(player, board.get(startPosition), board.get(nextPosition))
    } else {
      val overshotSpaces = nextPosition - board.endPosition
      Overshot(player, board.get(startPosition), board.get(board.endPosition), overshotSpaces)
    }
  }

  private def determineWinner(board: Board, move: BoardMove): Option[Player] = extractMove(move) match {
    case _: Overshot                                 => None
    case _ if move.end.position == board.endPosition => Some(move.player)
    case _                                           => None
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
