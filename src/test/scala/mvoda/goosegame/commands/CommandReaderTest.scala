package mvoda.goosegame.commands

import mvoda.goosegame.events.{ InvalidCommand, InvalidMove }
import mvoda.goosegame.game.Player
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CommandReaderTest extends AnyWordSpec with Matchers with EitherValues {
  "commandReader" should {
    "read valid add command" in {
      val input   = "add player Pippo"
      val command = CommandReader.read(input)
      command shouldBe Right(Add(Player("Pippo")))
    }

    "read valid move command" in {
      val input   = "move Pippo 1, 1"
      val command = CommandReader.read(input)
      command shouldBe Right(Move(Player("Pippo"), 1, 1))
    }

    "read move command and throw dice" in {
      val input   = "move Pippo"
      val command = CommandReader.read(input)
      command.isRight shouldBe true
      command.right.value shouldBe an[Move]
      command.right.value.asInstanceOf[Move].player shouldBe Player("Pippo")
    }

    "read commands regardless of case" in {
      val addInput  = "Add plAYer Pippo"
      val moveInput = "MovE Pippo 1, 1"
      val add       = CommandReader.read(addInput)
      val move      = CommandReader.read(moveInput)

      add shouldBe Right(Add(Player("Pippo")))
      move shouldBe Right(Move(Player("Pippo"), 1, 1))
    }

    "return error for invalid dice values" in {
      val input   = "move Pippo 0, 0"
      val command = CommandReader.read(input)
      command shouldBe Left(InvalidMove(0, 0))
    }

    "return error for invalid commands" in {
      val input   = "move player Pippo 0, 0"
      val command = CommandReader.read(input)
      command shouldBe Left(InvalidCommand(input))
    }

  }
}
