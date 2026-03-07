package gui4s.desktop.kit.widgets.decorator

import cats.Id
import cats.effect.*
import catnip.syntax.all.given 
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.kit.effects.{Draw, Place, PlacementEffect}
import gui4s.desktop.kit.widgets.{DesktopWidget, drawOnlyWidget}

extension[Event](widget : DesktopWidget[Event])
  def withDrawOnlyBackground(
    draw : Place[Draw],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]] =
    PlacementStrategy.Zip(
      OneElementPlacementStrategy.Center[PlacementEffect, Float],
      OneElementPlacementStrategy.Center[PlacementEffect, Float],
    )
  ): DesktopWidget[Event] =
    widget.withBackground(
      drawOnlyWidget[Event](draw),
      placement
    )
  end withDrawOnlyBackground
end extension