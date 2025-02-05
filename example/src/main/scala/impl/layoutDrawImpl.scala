package me.katze.gui4s.example
package impl

import api.impl.{DrawMonad, LayoutPlacementMeta}

import cats.Applicative
import me.katze.gui4s.widget.library.LayoutDraw
import cats.syntax.all.*

given layoutDrawImpl[Draww[_] : Applicative, MeasurementUnit](using drawMonad: DrawMonad[Draww, MeasurementUnit]): LayoutDraw[Draww[Unit], LayoutPlacementMeta[MeasurementUnit]] with
  override def drawChildren(children: List[(Draww[Unit], LayoutPlacementMeta[MeasurementUnit])]): Draww[Unit] =
    children.traverse_((childDraw, coordinates) => drawMonad.move(coordinates.x, coordinates.y, childDraw))
  end drawChildren
end layoutDrawImpl
