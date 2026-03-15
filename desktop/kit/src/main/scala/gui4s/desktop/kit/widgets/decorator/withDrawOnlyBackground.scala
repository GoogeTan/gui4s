package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.given
import cats.Id
import cats.effect._

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy

import gui4s.desktop.kit.effects.Draw
import gui4s.desktop.kit.effects.Place
import gui4s.desktop.kit.effects.PlacementEffect
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.drawOnlyWidget

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