package gui4s.android.kit.widgets.decorator

import cats.Id
import cats.effect.*
import catnip.syntax.all.given 
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.android.kit.effects.{Draw, Place, PlacementEffect}
import gui4s.android.kit.widgets.{AndroidWidget, drawOnlyWidget}

extension[Event](widget : AndroidWidget[Event])
  def withDrawOnlyBackground(
    draw : Place[Draw],
  ): AndroidWidget[Event] =
    withDrawOnlyBackground(
      draw,
      PlacementStrategy.Zip[
        PlacementEffect,
        Float,
        Float,
        Id,
        Float
      ](
        OneElementPlacementStrategy.Center[PlacementEffect, Float],
        OneElementPlacementStrategy.Center[PlacementEffect, Float],
      )
    )
  end withDrawOnlyBackground

  def withDrawOnlyBackground(
    draw : Place[Draw],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ): AndroidWidget[Event] =
    widget.withBackground(
      drawOnlyWidget[Event](draw),
      placement
    )
  end withDrawOnlyBackground
end extension
