package schess

/**
 * |rank|
 * |----------------------.
 * | 8  |                 |
 * | 7  |                 |
 * | 6  |                 |
 * | 5  |                 |
 * | 4  |                 |
 * | 3  |                 |
 * | 2  |                 |
 * | 1  |                 |
 * .----------------------.
 *      | a b c d e f g h | file
 *      ------------------
 */

case class Square(file: Int, rank: Int) {

  require(Square.isDefinedAt(file, rank))

  def color: Color = {
    val onEvenFile = if (file % 2 == 0) Black else White
    if (rank % 2 == 0) onEvenFile else Color.opposite(onEvenFile)
  }

  def delta(fileOffset: Int, rankOffset: Int): Option[Square] = {
    val (newFile, newRank) = (file + fileOffset, rank + rankOffset)
    if (Square.isDefinedAt(newFile, newRank))
      Some(Square(newFile, newRank))
    else
      None
  }

  def fileChar: Char = (('a' - 1) + file).toChar

  override def toString = fileChar.toString + rank

}

object Square {
  
  implicit def symbol2Square(symbol: Symbol): Square = Square(symbol)

  def apply(file: Char, rank: Int): Square = Square(file - ('a' - 1), rank)

  def apply(symbol: Symbol): Square = {
    require(symbol.name.length == 2)
    Square(symbol.name(0), symbol.name(1).toString.toInt)
  }

  def isDefinedAt(file: Int, rank: Int) = (file >= 1 && file <= 8 && rank >= 1 && rank <= 8)

}