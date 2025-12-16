package gui4s.core.widget.library

final case class TextRange private (start : Int, end : Int)

object TextRange:
  def apply(a : Int, b: Int) : TextRange =
    if a < b then
      new TextRange(a, b)
    else
      new TextRange(b, a)
    end if
  end apply

  def apply(a : Int) : TextRange =
    new TextRange(a, a)
  end apply

  extension (str : String)
    def replaceAt(at : TextRange, newSubstring : String) : String =
      str.substring(0, at.start) + newSubstring + str.substring(at.end)
    end replaceAt
  end extension
end TextRange
