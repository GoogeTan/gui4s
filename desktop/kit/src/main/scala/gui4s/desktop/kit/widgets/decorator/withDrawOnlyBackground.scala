package gui4s.desktop.kit.widgets.decorator

import cats.Id
import cats.effect.kernel.Sync
import catnip.syntax.all.given 
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.kit.effects.{Draw, Place, PlacementEffectC}
import gui4s.desktop.kit.widgets.{DesktopWidget, drawOnlyWidget}

extension[IO[_], Event](widget : DesktopWidget[IO, Event])
  def withDrawOnlyBackground(using Sync[IO])(
    draw : Place[IO, Draw[IO]],
    placement: PlacementStrategy[PlacementEffectC[IO], Rect[Float], Rect[Float], Id, Point2d[Float]] =
    PlacementStrategy.Zip(
      OneElementPlacementStrategy.Center[PlacementEffectC[IO], Float],
      OneElementPlacementStrategy.Center[PlacementEffectC[IO], Float],
    )
  ): DesktopWidget[IO, Event] =
    widget.withBackground(
      drawOnlyWidget(draw),
      placement
    )
  end withDrawOnlyBackground
end extension