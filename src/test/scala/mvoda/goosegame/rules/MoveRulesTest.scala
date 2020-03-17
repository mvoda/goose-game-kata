package mvoda.goosegame.rules

import mvoda.goosegame._
import mvoda.goosegame.moves._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MoveRulesTest extends AnyWordSpec with Matchers {
  private val board: Board  = Board()
  private val pippo: Player = Player("Pippo")
  private val pluto: Player = Player("Pluto")

  "computePlayerMove" should {
    "calculate final position when player does not hit the end of the board" in {
      val move = MoveRules.computePlayerMove(board, pippo, 0, 10)
      move shouldBe an[Advance]
      move.start.position shouldBe 0
      move.end.position shouldBe 10
    }

    "calculate final position when player does hits the end of the board" in {
      val move = MoveRules.computePlayerMove(board, pippo, 55, 10)
      move shouldBe an[Overshot]
      move.start.position shouldBe 55
      move.end.position shouldBe 63
    }
  }

  "prankedPlayerMove" should {
    val playerPositions = Map(pippo -> 10, pluto -> 15)

    "return pranked player to the other player's starting position" in {
      val move              = Advance(pippo, EmptySpace(10), EmptySpace(15))
      val prankedPlayerMove = MoveRules.prankedPlayerMove(playerPositions, move)
      prankedPlayerMove shouldBe Some(Return(pluto, move.end, move.start))
    }

    "return empty in case of not player collision" in {
      val move              = Advance(pippo, EmptySpace(10), EmptySpace(11))
      val prankedPlayerMove = MoveRules.prankedPlayerMove(playerPositions, move)
      prankedPlayerMove shouldBe None
    }
  }

  "updateGame" should {
    val game          = Game(Map(pippo -> 53), board)
    val initialUpdate = GameUpdate(game, Seq())

    "update player positions and log moves" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(55))
      val update = MoveRules.updateGame(initialUpdate, move)
      update.game.playerPositions(pippo) shouldBe move.end.position
      update.log.head shouldBe move
      update.game.winner shouldBe None
    }

    "detect winning condition when a player reaches the end" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(63))
      val update = MoveRules.updateGame(initialUpdate, move)
      update.game.winner shouldBe Some(move.player)
    }

    "ignore moves if game finished" in {
      val initialUpdate = GameUpdate(game.copy(winner = Some(pippo)), Seq())
      val move          = Advance(pippo, EmptySpace(53), EmptySpace(55))
      val update        = MoveRules.updateGame(initialUpdate, move)
      update shouldBe initialUpdate
    }
  }

  "processMove" should {
    val game          = Game(Map(pippo -> 53, pluto -> 55), board)
    val initialUpdate = GameUpdate(game, Seq())

    "update both the player and prankedPlayer position and log the moves" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(game.playerPositions(pluto)))
      val update = MoveRules.processMove(initialUpdate, move)
      update.game.playerPositions shouldBe Map(pippo -> 55, pluto -> 53)
      update.log.size shouldBe 2
    }

    "update just the player position and log the move in case of no collision" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(60))
      val update = MoveRules.processMove(initialUpdate, move)
      update.game.playerPositions shouldBe Map(pippo -> 60, pluto -> 55)
      update.log.size shouldBe 1
    }
  }

  "processMoveChain" should {
    val game          = Game(Map(pippo -> 0), board)
    val initialUpdate = GameUpdate(game, Seq())

    "bounces back after hitting the end of the board" in {
      val moveSpaces = 66
      val move       = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update     = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 60)
      update.log.size shouldBe 2
      update.log shouldBe Seq(
        Overshot(pippo, game.board.get(0), game.board.get(game.board.endPosition), moveSpaces - game.board.endPosition),
        Bounce(pippo, game.board.get(game.board.endPosition), game.board.get(60))
      )
    }

    "jump to target position if player lands on bridge" in {
      val moveSpaces = 6
      val move       = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update     = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 12)
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        Jump(pippo, game.board.get(moveSpaces), game.board.get(12))
      )
    }

    "move again if player lands on goose" in {
      val moveSpaces = 5
      val move       = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update     = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 10)
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        ExtraMove(Advance(pippo, game.board.get(moveSpaces), game.board.get(10)))
      )
    }

    "handles multiple bridge jumps" in {
      val customBoard   = Board(Seq(Start(0), Bridge(5, 10), Bridge(10, 20), Bridge(20, 40)), 63)
      val customGame    = game.copy(board = customBoard)
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 40)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Advance(pippo, customGame.board.get(0), customGame.board.get(moveSpaces)),
        Jump(pippo, customGame.board.get(moveSpaces), customGame.board.get(10)),
        Jump(pippo, customGame.board.get(10), customGame.board.get(20)),
        Jump(pippo, customGame.board.get(20), customGame.board.get(40))
      )
    }

    "handles multiple goose jumps" in {
      val customGame    = game.copy(playerPositions = Map(pippo -> 10))
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 4
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 10, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 22)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Advance(pippo, customGame.board.get(10), customGame.board.get(14)),
        ExtraMove(Advance(pippo, customGame.board.get(14), customGame.board.get(18))),
        ExtraMove(Advance(pippo, customGame.board.get(18), customGame.board.get(22)))
      )
    }

    "handles mixed bridge and goose jumps" in {
      val customBoard   = Board(Seq(Start(0), Bridge(5, 20), Goose(20), Bridge(25, 40)), 63)
      val customGame    = game.copy(board = customBoard)
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 40)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Advance(pippo, customGame.board.get(0), customGame.board.get(moveSpaces)),
        Jump(pippo, customGame.board.get(moveSpaces), customGame.board.get(20)),
        ExtraMove(Advance(pippo, customGame.board.get(20), customGame.board.get(25))),
        Jump(pippo, customGame.board.get(25), customGame.board.get(40))
      )
    }

    "handles bridge jump after bounce" in {
      val customBoard   = Board(Seq(Start(0), Bridge(2, 5)), 6)
      val customGame    = game.copy(board = customBoard)
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 10
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 5)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Overshot(pippo, customGame.board.get(0), customGame.board.get(customGame.board.endPosition), moveSpaces - customBoard.endPosition),
        Bounce(pippo, customGame.board.get(customGame.board.endPosition), customGame.board.get(2)),
        Jump(pippo, customGame.board.get(2), customGame.board.get(5))
      )
    }

    "handles goose jump after bounce" in {
      val customBoard   = Board(Seq(Start(0), Goose(60)), 63)
      val customGame    = game.copy(playerPositions = Map(pippo -> 62), board = customBoard)
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 4
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 62, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 62)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Overshot(pippo, customGame.board.get(62), customGame.board.get(customGame.board.endPosition), 3),
        Bounce(pippo, customGame.board.get(customGame.board.endPosition), customGame.board.get(60)),
        ExtraMove(Overshot(pippo, customGame.board.get(60), customGame.board.get(customGame.board.endPosition), 1)),
        Bounce(pippo, customGame.board.get(customGame.board.endPosition), customGame.board.get(62)),
      )
    }

    "stops extra moves in case of infinite loops" in {
      val customBoard   = Board(Seq(Start(0), Goose(10)), 15)
      val customGame    = game.copy(board = customBoard)
      val initialUpdate = GameUpdate(customGame, Seq())
      val moveSpaces    = 10
      val move          = MoveRules.computePlayerMove(customGame.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 10)
      update.log.size shouldBe 1
      update.log shouldBe Seq(
        Advance(pippo, customGame.board.get(0), customGame.board.get(moveSpaces))
      )
    }
  }

}
