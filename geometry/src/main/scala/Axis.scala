package me.katze.gui4s.geometry

import cats.kernel.Eq

enum Axis:
  case Vertical
  case Horizontal
  
  def another : Axis = this match
    case Axis.Vertical => Horizontal
    case Axis.Horizontal => Vertical
  end another
end Axis

given Eq[Axis] = Eq.fromUniversalEquals