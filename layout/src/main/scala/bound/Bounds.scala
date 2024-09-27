package me.katze.gui4s.layout
package bound

final case class Bounds[+T](horizontal : AxisBounds[T], vertical : AxisBounds[T])