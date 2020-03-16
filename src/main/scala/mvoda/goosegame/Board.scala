package mvoda.goosegame

case class Board(spaces: Map[Int, BoardSpace] = Board.defaultSpaces, endPosition: Int = 63) {
  def get(position: Int): BoardSpace = spaces(position)
}

object Board {
  val defaultSpaces: Map[Int, BoardSpace] = Map(
    6  -> Bridge(12),
    5  -> Goose,
    9  -> Goose,
    14 -> Goose,
    18 -> Goose,
    23 -> Goose,
    27 -> Goose
  ).withDefaultValue(EmptySpace)
}
