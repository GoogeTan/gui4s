package gui4s.core.widget.library

import cats.implicits.catsSyntaxOrder

import scala.math.Ordering.Implicits.infixOrderingOps

final case class TextPosition(line : Int, column : Int) extends Ordered[TextPosition]:
  def compare(that : TextPosition) : Int =
    (line, column).compare(that.line, that.column)
  end compare
end TextPosition

object TextPosition:
  val Zero : TextPosition = TextPosition(0, 0)
end TextPosition

final case class TextRange(start : TextPosition, end : TextPosition):
  def isCursor  : Boolean =
    start == end
  end isCursor

  def min : TextPosition =
    start.min(end)
  end min

  def max : TextPosition =
    start.max(end)
  end max
end TextRange

object TextRange:
  def apply(a : TextPosition) : TextRange =
    new TextRange(a, a)
  end apply
end TextRange
