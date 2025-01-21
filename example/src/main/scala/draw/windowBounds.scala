package me.katze.gui4s.example
package draw

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.*
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}

def windowBounds[F[_] : Functor, MU : Numeric](window: Window[F, MU]) : F[Bounds[MU]] =
  window.size
    .map((width, height) => Bounds(
      AxisBounds(None, Some(width)),
      AxisBounds(None, Some(height))
    ))
end windowBounds
