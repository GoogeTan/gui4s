package me.katze.gui4s.layout

import me.katze
final case class Placed[+MU, +T](value : T, x : MU, y : MU, width : MU, height : MU):
  def this(sized : Sized[MU, T], x : MU, y : MU) = this(sized.value, x, y, sized.width, sized.height)
  
  def axisCoordinate(axis : Axis) : MU =
    axis match
      case Axis.Vertical => y
      case Axis.Horizontal => x
    end match
  end axisCoordinate
  
  def axisValue(axis: Axis) : MU =
    axis match
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end axisValue
end Placed
