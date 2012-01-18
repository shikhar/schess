package schess

sealed trait Color

case object Black extends Color

case object White extends Color

object Color {

  def opposite(color: Color) = color match {
    case Black => White
    case White => Black
  }

}
