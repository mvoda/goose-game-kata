package mvoda.goosegame.commands

import mvoda.goosegame.{ GameError, InvalidCommand, InvalidMove, Player }

import scala.util.Random

object CommandReader {
  private val random     = new Random()
  private val firstDice  = "(\\d+),".r
  private val secondDice = "(\\d+)".r

  def read(input: String): Either[GameError, Command] = {
    val tokens = input.split("\\s+").toList
    tokens match {
      case ci"add" :: ci"player" :: name :: Nil                              => Right(Add(Player(name)))
      case ci"move" :: name :: Nil                                           => Right(Move(Player(name), throwDice(), throwDice()))
      case ci"move" :: name :: firstDice(first) :: secondDice(second) :: Nil => validateMove(name, first.toInt, second.toInt)
      case _                                                                 => Left(InvalidCommand(input))
    }
  }

  private def validateMove(name: String, firstDice: Int, secondDice: Int): Either[GameError, Command] = {
    if (firstDice <= 0 || firstDice > 6 || secondDice <= 0 || secondDice > 6) Left(InvalidMove(firstDice, secondDice))
    else Right(Move(Player(name), firstDice, secondDice))
  }

  private def throwDice(): Int = random.nextInt(5) + 1

  private implicit class CaseInsensitiveInterpolator(context: StringContext) {
    object CaseInsensitive {
      def unapply(other: String): Boolean = context.parts.mkString.equalsIgnoreCase(other)
    }
    def ci: CaseInsensitive.type = CaseInsensitive
  }

}
