package gui4s.core.widget.library.textfield

import scala.math.Ordering.Implicits.infixOrderingOps

import cats.Eq
import cats.derived.*
import cats.syntax.all.*

final case class TextRange(start : TextPosition, end : TextPosition) derives Eq:
  def isCursor  : Boolean =
    start === end
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
