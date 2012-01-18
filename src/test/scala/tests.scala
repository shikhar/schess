import org.specs2.mutable._

import schess._
import Square._

class StartingGameState extends Specification {

  val gs = GameState()
  val positions = gs.positions

  "The starting game" should {

    "have the kings at their place" in {
      positions must havePairs(Square('d1) -> King(White), Square('d8) -> King(Black))
    }

    "have the queens at their place" in {
      positions must havePairs(Square('e1) -> Queen(White), Square('e8) -> Queen(Black))
    }

    "have the rooks at their place" in {
      positions must havePairs(
        Square('a1) -> Rook(White),
        Square('h1) -> Rook(White),
        Square('a8) -> Rook(Black),
        Square('h8) -> Rook(Black)
      )
    }

    "have the knights at their place" in {
      positions must havePairs(
        Square('b1) -> Knight(White),
        Square('g1) -> Knight(White),
        Square('b8) -> Knight(Black),
        Square('g8) -> Knight(Black)
      )
    }

    "have the bishops at their place" in {
      positions must havePairs(
        Square('c1) -> Bishop(White),
        Square('f1) -> Bishop(White),
        Square('c8) -> Bishop(Black),
        Square('f8) -> Bishop(Black)
      )
    }

    "have the pawns in their places" in {
      ('a' to 'h').forall {
        file =>
          (positions.get(Square(file, 2)) must beSome(Pawn(White))) &&
            (positions.get(Square(file, 7)) must beSome(Pawn(Black)))
      }
    }

    "have the middle ranks free for battle" in {
      ('a' to 'h').forall {
        file => (3 to 6).forall {
          rank =>
            positions must not beDefinedAt (Square(file, rank))
        }
      }
    }

  }

}

class BasicMovement extends Specification {

  "On a starting game, a simple move from d2 to d4" should {

    val before = GameState()
    val move = Move(before, 'd2, 'd4)
    val after = move()

    "cause d2 to be unoccupied" in {
      after.positions must not beDefinedAt (Square('d2))
    }

    "cause d4 to have the occupant of d2" in {
      after.positions.get('d4) must_== before.positions.get('d2)
    }

    ".. and a further move from e2 to d4 should capture d4" in {
      Move(after, 'e7, 'd4)().positions.get('d4) must_== after.positions.get('e7)
    }

  }

}


class KingMoves extends Specification {

  val king = King(White)

  "Given a free 1-step radius around him, the king" can {

    val gs = GameState(Map(Square('e5) -> king))

    val moves = king.moves('e5, gs)

    "step freely in it, and only those steps" in {

      moves.map(_.to) must haveTheSameElementsAs(Seq(
        'd6, 'd5, 'd4, 'e4, 'f6, 'f5, 'f4, 'e6
      ).map(Square(_)))

    }

  }

  "If he's surrounded on some spots, the king" can {

    val imaginaryRook = Rook(White)

    val gs = GameState(Map(Square('e5) -> king)
      ++ Seq('d5, 'd4, 'e4, 'f4, 'f5, 'f6).map(Square(_) -> imaginaryRook))

    "still move to others" in {

      king.moves('e5, gs).map(_.to) must haveTheSameElementsAs(Seq('d6, 'e6).map(Square(_)))

    }

  }

}


class QueenMoves extends Specification {

  val queen = Queen(White)

  "Given a free board, the queen" can {

    val gs = GameState(Map(Square('c4) -> queen))

    "move freely in straight lines" in {

      queen.moves('c4, gs).map(_.to) must haveTheSameElementsAs(Seq(
        'd4, 'e4, 'f4, 'g4, 'h4, // east
        'a4, 'b4, // west
        'c5, 'c6, 'c7, 'c8, // north
        'c1, 'c2, 'c3, // south

        'd5, 'e6, 'f7, 'g8, // north east
        'b5, 'a6, // north west
        'b3, 'a2, // south west
        'd3, 'e2, 'f1 // south east
      ).map(Square(_)))
    }

  }

  "Given obstacle her path, the queen" can {

    val (ourPawn, enemeyPawn) = (Pawn(White), Pawn(Black))

    val gs = GameState(Map(
      Square('b2) -> queen,
      Square('a2) -> ourPawn, // west
      Square('b1) -> ourPawn, // south
      Square('a1) -> ourPawn, // south west
      Square('a3) -> ourPawn, // north west
      Square('c1) -> ourPawn, // south east
      Square('c2) -> ourPawn, //east
      Square('e5) -> enemeyPawn, // north east
      Square('b4) -> enemeyPawn // north
    )
    )

    val toSquares = queen.moves('b2, gs).map(_.to)

    "not step on her own" in {

      Seq('b1, 'a1, 'a3, 'c1, 'c2).forall {
        toSquares must not contain Square(_)
      }

    }

    "capture enemies, but not go beyond" in {

      Seq('e5, 'b4).forall {
        sq => toSquares must contain(Square(sq))
      }

      Seq('f6, 'b5).forall {
        toSquares must not contain Square(_)
      }

    }

  }

}

class KnightMoves extends Specification {

  val knight = Knight(Black)

  "Given a knight on an empty board, he" can {

    "jump around to any of the nearest square not on the same rank, file, or diagonal" in {

      val gs = GameState(Map(Square('d4) -> knight))

      knight.moves('d4, gs).map(_.to) must haveTheSameElementsAs(Set(
        'c2, 'b3, 'b5, 'c6, 'e6, 'f5, 'f3, 'e2
      ).map(Square(_)))

    }
  }

  "In case of occupancy at his spots, our hero" should {

    "be friendly to his own" in {

      val gs = GameState(Map(Square('d4) -> knight) ++
        Seq('c2, 'b3, 'b5, 'c6, 'e6, 'f5, 'f3).map(Square(_) -> Pawn(knight.color))
      )

      knight.moves('d4, gs).map(_.to) must haveTheSameElementsAs(Seq(Square('e2)))

    }

    "but capture enemeies" in {

      val gs = GameState(Map(
        Square('d4) -> knight,
        Square('f3) -> Pawn(Color.opposite(knight.color))
      ) ++ Seq('c2, 'b3, 'b5, 'c6, 'e6, 'f5).map(Square(_) -> Pawn(knight.color))
      )

      knight.moves('d4, gs).map(_.to) must contain(Square('f3))

    }

  }

}

class PawnMoves extends Specification {

  val pawn = Pawn(White)

  "Given a pawn is at his home position, he" can {

    val gs = GameState()
    val toSquares = pawn.moves('a2, gs).map(_.to)

    "move 1 or 2 steps forward" in {

      toSquares must haveTheSameElementsAs(Seq('a3, 'a4).map(Square(_)))

    }

  }

  "On the battlefield, he" can {

    val enemeyRook = Rook(Black)
    val gs = GameState(Map(Square('c6) -> pawn, Square('b7) -> enemeyRook, Square('d7) -> enemeyRook))
    val toSquares = pawn.moves('c6, gs).map(_.to)

    "capture diagonally or skip forward" in {

      toSquares must haveTheSameElementsAs(Seq('b7, 'c7, 'd7).map(Square(_)))

    }

  }

}