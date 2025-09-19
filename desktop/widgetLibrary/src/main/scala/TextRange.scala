package gui4s.desktop.widget.library

final case class TextRange private (beginning : Int, ending : Int)

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
      str.substring(0, at.beginning) + newSubstring + str.substring(at.ending)
    end replaceAt
  end extension
end TextRange
