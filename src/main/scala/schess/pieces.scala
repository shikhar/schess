package schess

sealed trait Piece {

  def color: Color

  def homeFiles: Seq[Char]

  def homeRank: Int

  def moves(from: Square, state: GameState): Set[Move]

  def threatens(from: Square, state: GameState, where: Square) = {
    state.positions.get(where).exists(_.color != color) &&
      moves(from, state).exists(_.captures.exists(_._1 == where))
  }
  
}

case class King(color: Color) extends Piece {

  override val homeFiles = Seq('d')

  override val homeRank = color match {
    case White => 1
    case Black => 8
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))
    Set() ++ {
      for {
        fileOffset <- (-1 to 1)
        rankOffset <- (-1 to 1)
        to <- from.delta(fileOffset, rankOffset)
        if state.positions.get(to).map(_.color != color).getOrElse(true)
      } yield new Move(state, from, to)
    } ++ castling(from, state)
  }

  def inCheck(at: Square,  state: GameState) =
    state.positions.exists(position => position._2.threatens(position._1, state, at))

  def castling(from: Square, state: GameState): Seq[Move] = {
    if (homeRank == from.rank
      && homeFiles.contains(from.file)
      && !state.journal.exists(_.piece == this)
      && !inCheck(from, state)) {

      val lm = new LineMovement(from, state)
      for {

        rookHome <- Rook(color).homeFiles.map(Square(_, homeRank))
        if !state.journal.exists(_.from == rookHome)

        (kingToRookDirection, kingToRookPath) <- Seq((-1, lm.west _), (1, lm.east _))
        rookAdjacent <- rookHome.delta(kingToRookDirection * -1, homeRank)
        if kingToRookPath().contains(rookAdjacent)

        kingDest <- from.delta(2 * kingToRookDirection, homeRank)
        rookDest <- from.delta(kingToRookDirection, homeRank)

      } yield new Move(state, from, kingDest) with Castling {
        override val rookFrom = rookHome
        override val rookTo = rookDest
      }

    } else
      Seq()
  }

}

case class Queen(color: Color) extends Piece {

  override val homeFiles = Seq('e')

  override val homeRank = color match {
    case White => 1
    case Black => 8
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))
    val lm = new LineMovement(from, state)
    Set(
      lm.west, lm.east, lm.north, lm.south,
      lm.northEast, lm.northWest, lm.southWest, lm.southEast
    ).flatMap(_.map(Move(state, from, _)))
  }

}

case class Rook(color: Color) extends Piece {

  override val homeFiles = Seq('a', 'h')

  override val homeRank = color match {
    case White => 1
    case Black => 8
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))
    val lm = new LineMovement(from, state)
    Set(lm.west, lm.east, lm.north, lm.south).flatMap(_.map(Move(state, from, _)))
  }

}

case class Bishop(color: Color) extends Piece {

  override val homeFiles = Seq('c', 'f')

  override val homeRank = color match {
    case White => 1
    case Black => 8
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))
    val lm = new LineMovement(from, state)
    Set(lm.northEast, lm.northWest, lm.southWest, lm.southEast).flatMap(_.map(Move(state, from, _)))
  }

}

case class Knight(color: Color) extends Piece {

  override val homeFiles = Seq('b', 'g')

  override val homeRank = color match {
    case White => 1
    case Black => 8
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))
    for {
      (basisFile, basisRank) <- Set((1, 2), (2, 1))
      (adjustFile, adjustRank) <- Set((-1, -1), (-1, 1), (1, 1), (1, -1))
      to <- from.delta(basisFile * adjustFile, basisRank * adjustRank)
      if state.positions.get(to).map(_.color != color).getOrElse(true)
    } yield Move(state, from, to)
  }

}

case class Pawn(color: Color) extends Piece {

  override val homeFiles = ('a' to 'h')

  override val homeRank = color match {
    case White => 2
    case Black => 7
  }

  val direction = color match {
    case White => 1
    case Black => -1
  }

  override def moves(from: Square, state: GameState): Set[Move] = {
    assert(state.positions.get(from) == Some(this))

    def filterOccupied(square: Option[Square]) = square.filter(!state.positions.isDefinedAt(_))

    val oneStep = filterOccupied(from.delta(0, direction * 1))

    val twoSteps =
      if (from.rank == homeRank && oneStep.isDefined)
        filterOccupied(from.delta(0, direction * 2))
      else
        None

    val capturingSteps = for {
      (fileOffset, rankOffset) <- List((-1, 1), (1, 1))
      to <- from.delta(fileOffset, direction * rankOffset)
      if state.positions.get(to).exists(_.color != color)
    } yield to

    val opposingQueen = Queen(Color.opposite(color))

    (Set() ++ oneStep ++ twoSteps ++ capturingSteps).map( to =>
      if (to.rank == opposingQueen.homeRank)
        new Move(state, from, to) with PawnPromotion
      else
        Move(state, from, to)
    ) ++ enPassant(from, state)
  }

  def enPassant(from: Square, state: GameState): Option[Move] = {

    val opponent = Pawn(Color.opposite(color))

    state.journal match {

      case pastMove :: _
        if (pastMove.piece == opponent
          && pastMove.from.rank == opponent.homeRank 
          && math.abs(pastMove.to.rank - pastMove.from.rank) == 2
          && pastMove.to.rank == from.rank 
          && math.abs(from.file - pastMove.to.file) == 1) =>

        Some(new Move(state, from, from.delta(pastMove.to.file, direction * 1).get) {
          override val captures = Some(pastMove.to, opponent)
        })

      case _ =>
        None

    }

  }

}

class LineMovement(val from: Square, val state: GameState) {

  import LineMovement._

  private lazy val color = state.positions(from).color

  private def along(offsets: Seq[(Int, Int)]): Seq[Square] = {
    var pathBlocked = false
    for {
      (file, rank) <- offsets
      to <- from.delta(file, rank)
      occupier = state.positions.get(to)
      if !pathBlocked && occupier.map(_.color != color).getOrElse(true)
    } yield {
      pathBlocked = pathBlocked || occupier.isDefined
      to
    }
  }

  def east = along(increasingOffsets zip zeroStream)

  def west = along(decreasingOffsets zip zeroStream)

  def north = along(zeroStream zip increasingOffsets)

  def south = along(zeroStream zip decreasingOffsets)

  def northEast = along(increasingOffsets zip increasingOffsets)

  def northWest = along(decreasingOffsets zip increasingOffsets)

  def southWest = along(increasingOffsets zip decreasingOffsets)

  def southEast = along(decreasingOffsets zip decreasingOffsets)

}

object LineMovement {

  private def zeroStream: Stream[Int] = Stream.cons(0, zeroStream)

  private lazy val increasingOffsets = 1 to 7

  private lazy val decreasingOffsets = -1 to -7 by -1

}