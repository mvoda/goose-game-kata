package mvoda.goosegame.rules

import mvoda.goosegame.commands.Move
import mvoda.goosegame.events._
import mvoda.goosegame.game.{ GameUpdate, _ }
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MoveRulesTest extends AnyWordSpec with Matchers with EitherValues {
  private val pippo: Player = Player("Pippo")
  private val pluto: Player = Player("Pluto")

  "computePlayerMove" should {
    "calculate final position when player does not hit the end of the board" in {
      val move = MoveRules.computePlayerMove(Board(), pippo, 0, 10)
      move shouldBe an[Advance]
      move.start.position shouldBe 0
      move.end.position shouldBe 10
    }

    "calculate final position when player does hits the end of the board" in {
      val move = MoveRules.computePlayerMove(Board(), pippo, 55, 10)
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
    val game          = Game(Map(pippo -> 53), Board())
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

    "not detect winning condition when a player overshoots the end" in {
      val move   = Overshot(pippo, EmptySpace(53), EmptySpace(63), 3)
      val update = MoveRules.updateGame(initialUpdate, move)
      update.game.winner shouldBe None
    }

    "detect winning condition when a player reaches the end as a result of a return move" in {
      val move   = Return(pippo, EmptySpace(53), EmptySpace(63))
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
    val game          = Game(Map(pippo -> 53, pluto -> 55), Board())
    val initialUpdate = GameUpdate(game, Seq())

    "update both the player and prankedPlayer position and log the moves" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(game.playerPositions(pluto)))
      val update = MoveRules.processMove(initialUpdate, move)
      update.game.playerPositions shouldBe Map(pippo -> 55, pluto -> 53)
      update.log shouldBe Seq(
        move,
        Return(pluto, game.board.get(55), game.board.get(53))
      )
    }

    "update just the player position and log the move in case of no collision" in {
      val move   = Advance(pippo, EmptySpace(53), EmptySpace(60))
      val update = MoveRules.processMove(initialUpdate, move)
      update.game.playerPositions shouldBe Map(pippo -> 60, pluto -> 55)
      update.log shouldBe Seq(move)
    }

    "detect end of the game after a swap places the player on the end space" in {
      val game          = Game(Map(pippo -> 63, pluto -> 60), Board())
      val initialUpdate = GameUpdate(game, Seq())
      val move          = Bounce(pippo, EmptySpace(63), EmptySpace(60))
      val update        = MoveRules.processMove(initialUpdate, move)
      update.game.playerPositions shouldBe Map(pippo -> 60, pluto -> 63)
      update.game.winner shouldBe Some(pluto)
      update.log shouldBe Seq(
        move,
        Return(pluto, game.board.get(60), game.board.get(63))
      )
    }
  }

  "processMoveChain" should {
    "bounces back after hitting the end of the board" in {
      val game          = Game(Map(pippo -> 0), Board())
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 66
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 60)
      update.log.size shouldBe 2
      update.log shouldBe Seq(
        Overshot(pippo, game.board.get(0), game.board.get(game.board.endPosition), moveSpaces - game.board.endPosition),
        Bounce(pippo, game.board.get(game.board.endPosition), game.board.get(60))
      )
    }

    "jump to target position if player lands on bridge" in {
      val game          = Game(Map(pippo -> 0), Board())
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 6
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 12)
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        Jump(pippo, game.board.get(moveSpaces), game.board.get(12))
      )
    }

    "move again if player lands on goose" in {
      val game          = Game(Map(pippo -> 0), Board())
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 10)
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        ExtraMove(Advance(pippo, game.board.get(moveSpaces), game.board.get(10)))
      )
    }

    "handles multiple bridge jumps" in {
      val customBoard   = Board(Seq(Start(0), Bridge(5, 10), Bridge(10, 20), Bridge(20, 40)), 63)
      val game          = Game(Map(pippo -> 0), customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 40)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        Jump(pippo, game.board.get(moveSpaces), game.board.get(10)),
        Jump(pippo, game.board.get(10), game.board.get(20)),
        Jump(pippo, game.board.get(20), game.board.get(40))
      )
    }

    "handles multiple goose jumps" in {
      val game          = Game(playerPositions = Map(pippo -> 10), Board())
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 4
      val move          = MoveRules.computePlayerMove(game.board, pippo, 10, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 22)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(10), game.board.get(14)),
        ExtraMove(Advance(pippo, game.board.get(14), game.board.get(18))),
        ExtraMove(Advance(pippo, game.board.get(18), game.board.get(22)))
      )
    }

    "handles mixed bridge and goose jumps" in {
      val customBoard   = Board(Seq(Start(0), Bridge(5, 20), Goose(20), Bridge(25, 40)), 63)
      val game          = Game(Map(pippo -> 0), customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 40)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        Jump(pippo, game.board.get(moveSpaces), game.board.get(20)),
        ExtraMove(Advance(pippo, game.board.get(20), game.board.get(25))),
        Jump(pippo, game.board.get(25), game.board.get(40))
      )
    }

    "handles bridge jump after bounce" in {
      val customBoard   = Board(Seq(Start(0), Bridge(2, 5)), 6)
      val game          = Game(Map(pippo -> 0), customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 10
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 5)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Overshot(pippo, game.board.get(0), game.board.get(game.board.endPosition), moveSpaces - customBoard.endPosition),
        Bounce(pippo, game.board.get(game.board.endPosition), game.board.get(2)),
        Jump(pippo, game.board.get(2), game.board.get(5))
      )
    }

    "handles goose jump after bounce" in {
      val customBoard   = Board(Seq(Start(0), Goose(60)), 63)
      val game          = Game(playerPositions = Map(pippo -> 62), board = customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 4
      val move          = MoveRules.computePlayerMove(game.board, pippo, 62, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 62)
      update.log.size shouldBe 4
      update.log shouldBe Seq(
        Overshot(pippo, game.board.get(62), game.board.get(game.board.endPosition), 3),
        Bounce(pippo, game.board.get(game.board.endPosition), game.board.get(60)),
        ExtraMove(Overshot(pippo, game.board.get(60), game.board.get(game.board.endPosition), 1)),
        Bounce(pippo, game.board.get(game.board.endPosition), game.board.get(62)),
      )
    }

    "stops extra moves in case of infinite loops" in {
      val customBoard   = Board(Seq(Start(0), Goose(10)), 15)
      val game          = Game(Map(pippo -> 0), customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 10
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 10)
      update.log.size shouldBe 1
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces))
      )
    }

    "move the pranked player to the bridge if the collision happened after jump" in {
      val customBoard   = Board(Seq(Start(0), Bridge(10, 15)), 20)
      val game          = Game(playerPositions = Map(pippo -> 0, pluto -> 15), board = customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 10
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 15, pluto -> 10)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        Jump(pippo, game.board.get(moveSpaces), game.board.get(15)),
        Return(pluto, game.board.get(15), game.board.get(10))
      )
    }

    "move the pranked player to the goose if the collision happened after extra move" in {
      val customBoard   = Board(Seq(Start(0), Goose(5)), 20)
      val game          = Game(playerPositions = Map(pippo -> 0, pluto -> 10), board = customBoard)
      val initialUpdate = GameUpdate(game, Seq())
      val moveSpaces    = 5
      val move          = MoveRules.computePlayerMove(game.board, pippo, 0, moveSpaces)
      val update        = MoveRules.processMoveChain(initialUpdate, move, moveSpaces)
      update.game.playerPositions shouldBe Map(pippo -> 10, pluto -> 5)
      update.log.size shouldBe 3
      update.log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(moveSpaces)),
        ExtraMove(Advance(pippo, game.board.get(moveSpaces), game.board.get(10))),
        Return(pluto, game.board.get(10), game.board.get(5))
      )
    }
  }

  "process" should {
    "play the game and log the moves" in {
      val game    = Game(Map(pippo -> 0, pluto -> 0), Board())
      val update1 = MoveRules.process(game, pippo, 6)
      val update2 = MoveRules.process(update1.game, pluto, 5)
      update2.game.playerPositions shouldBe Map(pippo -> 12, pluto -> 10)
      val log = update1.log ++ update2.log
      log shouldBe Seq(
        Advance(pippo, game.board.get(0), game.board.get(6)),
        Jump(pippo, game.board.get(6), game.board.get(12)),
        Advance(pluto, game.board.get(0), game.board.get(5)),
        ExtraMove(Advance(pluto, game.board.get(5), game.board.get(10)))
      )
    }
  }

  "movePlayer" should {
    val game = Game(Map(pippo -> 0, pluto -> 0), Board())

    "apply the move if player exists" in {
      val result = MoveRules.movePlayer(game, Move(pippo, 1, 1))
      val update = result.right.value
      update.game.playerPositions shouldBe Map(pippo -> 2, pluto -> 0)
      update.log shouldBe Seq(
        PlayerRolls(pippo, 1, 1),
        Advance(pippo, game.board.get(0), game.board.get(2))
      )
    }

    "not change the game state if player does not exist" in {
      val missingPlayer = Player("whoops")
      val result        = MoveRules.movePlayer(game, Move(missingPlayer, 3, 3))
      result.left.value shouldBe PlayerDoesNotExist(missingPlayer)
    }

    "log winner after a forward move" in {
      val game   = Game(Map(pippo -> 61), Board())
      val result = MoveRules.movePlayer(game, Move(pippo, 1, 1))
      val update = result.right.value
      update.game.playerPositions shouldBe Map(pippo -> 63)
      update.log shouldBe Seq(
        PlayerRolls(pippo, 1, 1),
        Advance(pippo, game.board.get(61), game.board.get(63)),
        PlayerWins(pippo)
      )
    }

    "log correct winner after a collision move" in {
      val game   = Game(Map(pippo -> 61, pluto -> 62), Board())
      val result = MoveRules.movePlayer(game, Move(pippo, 1, 2))
      val update = result.right.value
      update.game.playerPositions shouldBe Map(pippo -> 62, pluto -> 63)
      update.log shouldBe Seq(
        PlayerRolls(pippo, 1, 2),
        Overshot(pippo, game.board.get(61), game.board.get(63), 1),
        Bounce(pippo, game.board.get(63), game.board.get(62)),
        Return(pluto, game.board.get(62), game.board.get(63)),
        PlayerWins(pluto)
      )
    }
  }

}
