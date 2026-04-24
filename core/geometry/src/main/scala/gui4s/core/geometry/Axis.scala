package gui4s.core.geometry

import cats.derived.*
import cats.kernel.Eq

enum Axis derives Eq:
  case Vertical
  case Horizontal
  
  def another : Axis = this match
    case Axis.Vertical => Horizontal
    case Axis.Horizontal => Vertical
  end another
end Axis