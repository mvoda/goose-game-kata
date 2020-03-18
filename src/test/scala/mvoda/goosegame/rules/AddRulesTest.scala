package mvoda.goosegame.rules

import mvoda.goosegame.events.{ ExistingPlayers, PlayerAlreadyExists }
import mvoda.goosegame.game.{ Board, Game, Player }
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AddRulesTest extends AnyWordSpec with Matchers with EitherValues {
  private val pippo: Player = Player("Pippo")

  "addPlayer" should {
    "add new players to the game" in {
      val game   = Game(Map(), Board())
      val result = AddRules.addPlayer(game, pippo)
      val update = result.right.value
      update.game.playerPositions.size shouldBe game.playerPositions.size + 1
      update.log shouldBe Seq(ExistingPlayers(Set(pippo)))
    }

    "not add players if they already exist" in {
      val game   = Game(Map(pippo -> 0), Board())
      val result = AddRules.addPlayer(game, pippo)
      result.left.value shouldBe PlayerAlreadyExists(pippo)
    }
  }
}
