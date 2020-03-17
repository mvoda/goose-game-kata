package mvoda.goosegame.rules

import mvoda.goosegame._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AddRulesTest extends AnyWordSpec with Matchers {
  private val pippo: Player = Player("Pippo")

  "addPlayer" should {
    "add new players to the game" in {
      val game   = Game(Map(), Board())
      val update = AddRules.addPlayer(game, pippo)
      update.game.playerPositions.size shouldBe game.playerPositions.size + 1
      update.log shouldBe Seq(ExistingPlayers(Set(pippo)))
    }

    "not add players if they already exist" in {
      val game   = Game(Map(pippo -> 0), Board())
      val update = AddRules.addPlayer(game, pippo)
      update.game.playerPositions.size shouldBe game.playerPositions.size
      update.log shouldBe Seq(PlayerAlreadyExists(pippo))
    }
  }
}
