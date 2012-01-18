package schess

case class PastMove(piece: Piece, from: Square, to: Square, captured: Option[(Square, Piece)] = None,
                     promotion: Option[Piece] = None, castled: Boolean = false) {

  def enPassant = captured.exists(_._1 != to)

  override def toString: String = {
    import collection.mutable.StringBuilder
    val sb = new StringBuilder()
    sb.append(piece).append(" from ").append(from).append(" to ").append(to);
    if (castled) sb.append("; by way of castling")
    promotion.foreach(sb.append("; promoted to ").append(_))
    captured.foreach(c => sb.append("; capturing ").append(c._2).append(" from ").append(c._1))
    sb.toString()
  }

}

case class GameState(positions: GameState.Positions, journal: List[PastMove] = Nil)

case class Army(color: Color) {
  val pieces = List(King, Queen, Rook, Bishop, Knight, Pawn).map(_(color))
}

object GameState {

  type Positions = Map[Square, Piece]

  // Starting off
  def apply(): GameState = {
    val positions: Positions = Map() ++ {
      for {
        army <- Seq(Army(White), Army(Black))
        piece <- army.pieces
        square <- piece.homeFiles.map(Square(_, piece.homeRank))
      } yield (square -> piece)
    }
    GameState(positions)
  }

}
