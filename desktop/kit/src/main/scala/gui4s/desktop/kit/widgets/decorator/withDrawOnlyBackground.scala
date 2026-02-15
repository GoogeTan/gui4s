package gui4s.desktop.kit.widgets.decorator

import cats.Id
import cats.effect.*
import catnip.syntax.all.given 
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.kit.effects.{Draw, Place, PlacementEffectC}
import gui4s.desktop.kit.widgets.{DesktopWidget, drawOnlyWidget}

extension[Event](widget : DesktopWidget[Event])
  def withDrawOnlyBackground(
    draw : Place[IO, Draw[IO]],
    placement: PlacementStrategy[PlacementEffectC[IO], Rect[Float], Rect[Float], Id, Point2d[Float]] =
    PlacementStrategy.Zip(
      OneElementPlacementStrategy.Center[PlacementEffectC[IO], Float],
      OneElementPlacementStrategy.Center[PlacementEffectC[IO], Float],
    )
  ): DesktopWidget[Event] =
    widget.withBackground(
      drawOnlyWidget[Event](draw),
      placement
    )
  end withDrawOnlyBackground
end extension