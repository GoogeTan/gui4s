package me.katze.gui4s.layout
package bound

final case class Bounds[+T : Numeric](horizontal : AxisBounds[T], vertical : AxisBounds[T]):
  def this(width : T, height : T) =
    this(new AxisBounds(width), new AxisBounds(height))
  end this
end Bounds
