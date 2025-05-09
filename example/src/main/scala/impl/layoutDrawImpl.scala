package me.katze.gui4s.example
package impl

import api.{DrawMonad, LayoutPlacementMeta}

import cats.Monoid
import cats.syntax.all.*
import me.katze.gui4s.widget.library.LayoutDraw

given layoutDrawImpl[Draw : Monoid, MeasurementUnit](using drawMonad: DrawMonad[Draw, MeasurementUnit]): LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]] with
  override def drawChildren(children: List[(Draw, LayoutPlacementMeta[MeasurementUnit])]): Draw =
    children.foldMap((childDraw, coordinates) => drawMonad.drawAt(coordinates.x, coordinates.y, childDraw))
  end drawChildren
end layoutDrawImpl
