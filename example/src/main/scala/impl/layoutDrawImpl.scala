package me.katze.gui4s.example
package impl

import api.impl.{DrawMonad, LayoutPlacementMeta}

import cats.{Applicative, Monoid}
import me.katze.gui4s.widget.library.LayoutDraw
import cats.syntax.all.*
import cats.syntax.all.*

given layoutDrawImpl[Draw : Monoid, MeasurementUnit](using drawMonad: DrawMonad[Draw, MeasurementUnit]): LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]] with
  override def drawChildren(children: List[(Draw, LayoutPlacementMeta[MeasurementUnit])]): Draw =
    children.foldMap((childDraw, coordinates) => drawMonad.move(coordinates.x, coordinates.y, childDraw))
  end drawChildren
end layoutDrawImpl
