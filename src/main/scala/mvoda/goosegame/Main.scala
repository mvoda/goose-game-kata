package mvoda.goosegame

import mvoda.goosegame.commands.CommandReader
import mvoda.goosegame.events.GameMessage
import mvoda.goosegame.game.{ Game, GameUpdate }
import mvoda.goosegame.rules.GameRules

import scala.annotation.tailrec

object Main {
  private val quitCommand: String = ":q"

  def main(args: Array[String]): Unit = {
    println(s"Game started. Type $quitCommand to quit.")
    mainLoop(Game())
  }

  @tailrec
  private def mainLoop(game: Game): Unit = {
    val input = io.StdIn.readLine
    if (input != quitCommand) {
      val updateEither = for {
        command <- CommandReader.read(input)
        update = GameRules.applyCommand(game, command)
      } yield update
      val gameUpdate = updateEither.fold(error => GameUpdate(game, Seq(error)), commandUpdate => commandUpdate)
      printLog(gameUpdate.log)
      if (gameUpdate.game.winner.isEmpty) mainLoop(gameUpdate.game)
    }
  }

  private def printLog(log: Seq[GameMessage]): Unit = {
    val logString = log.map(_.msg).mkString(" ")
    println(logString)
  }
}
