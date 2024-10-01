package me.katze.gui4s.example
package draw

import place.RunPlacement

import cats.*
import me.katze.gui4s.layout.Measurable
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.{*, given}

def swingBounds[F[_] : Functor, MU : Numeric](api : SwingApi[F, MU]) : F[Bounds[MU]] =
  api.size
    .map((width, height) => Bounds(
      AxisBounds(None, Some(width)),
      AxisBounds(None, Some(height))
    ))
end swingBounds
