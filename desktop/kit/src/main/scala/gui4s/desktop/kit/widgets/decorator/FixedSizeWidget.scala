package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.given
import cats.*
import cats.effect.*
import gui4s.core.geometry.{Axis, InfinityOr, Rect}
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.widgets.{DesktopWidget, linearContainerWidget}

def fixedSizeWidget[
  Event
](
   size : Rect[Float],
   horizontalPlacement : OneElementLinearContainerPlacementStrategy,
   verticalPlacement : OneElementLinearContainerPlacementStrategy
) : Decorator[DesktopWidget[Event]] =
  gui4s.desktop.widget.library.decorator.fixedSizeWidget(
      Place.withBoundsK(
        _ => size.map(new InfinityOr(_))
      ),
    linearContainerWidget[Event, Id],
    Axis.Horizontal,
    horizontalPlacement,
    verticalPlacement
  )
end fixedSizeWidget