package mvoda.goosegame

case class Board(spaces: Map[Int, BoardSpace], endPosition: Int) {
  def get(position: Int): BoardSpace = spaces(position)
}

object Board {
  val defaultBoardSpaces: Seq[BoardSpace] = Seq(Start(0), Bridge(6, 12), Goose(5), Goose(9), Goose(14), Goose(18), Goose(23), Goose(27))

  def apply(spaces: Seq[BoardSpace], endPosition: Int): Board = {
    val spaceMap = spaces.map(space => space.position -> space).toMap.withDefault(EmptySpace)
    new Board(spaceMap, endPosition)
  }

  def apply(): Board = Board(defaultBoardSpaces, 63)
}
