package me.katze.gui4s.layout

// TODO имена получше для методов. Не отражают суть.
final case class Sized[+MU, +T](value : T, width : MU, height : MU):
  def mainAxisValue(axis : Axis) : MU =
    axis match
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end mainAxisValue
  
  def additionalAxisValue(axis: Axis): MU =
    axis match
      case Axis.Vertical => width
      case Axis.Horizontal => height
    end match
  end additionalAxisValue
end Sized
