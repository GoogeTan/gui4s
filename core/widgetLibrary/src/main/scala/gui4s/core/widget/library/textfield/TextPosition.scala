package gui4s.core.widget.library.textfield

import cats.syntax.order.*

final case class TextPosition(line : Int, column : Int) extends Ordered[TextPosition]:
  def compare(that : TextPosition) : Int =
    (line, column).compare(that.line, that.column)
  end compare
end TextPosition

object TextPosition:
  val Zero : TextPosition = TextPosition(0, 0)
end TextPosition
