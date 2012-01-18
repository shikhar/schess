package schess

trait BasicMove {

  def state: GameState

  def from: Square

  def to: Square

  def piece = state.positions(from)

  def captures: Option[(Square, Piece)] = state.positions.get(to).flatMap(capturedPiece =>
    if (capturedPiece.color != piece.color) Some(to, capturedPiece) else None
  )

  def toPast: PastMove = PastMove(piece, from, to, captures)

  def apply(): GameState = GameState(
    state.positions -- captures.flatMap(x => Some(x._1)) - from + (to -> piece),
    toPast :: state.journal
  )

}

trait Castling extends BasicMove {

  def rookFrom: Square

  def rookTo: Square

  override abstract def toPast: PastMove = super.toPast.copy(castled = true)

  override abstract def apply(): GameState = new Move(super.apply, rookFrom, rookTo).apply

}

trait PawnPromotion extends BasicMove {

  override abstract def apply(): GameState = apply(Queen(piece.color))

  def apply(desired: Piece): GameState = {
    desired match {
      case Queen(c) if c == piece.color =>
      case Knight(c) if c == piece.color =>
      case Rook(c) if c == piece.color =>
      case Bishop(c) if c == piece.color =>
      case x => sys.error("Can't be promoted to " + x)
    }

    val basic = super.apply
    basic.copy(
      positions = basic.positions + (to -> desired),
      journal = basic.journal.head.copy(promotion = Some(desired)) :: basic.journal.tail
    )
  }

}

case class Move(state: GameState, from: Square, to: Square) extends BasicMove
