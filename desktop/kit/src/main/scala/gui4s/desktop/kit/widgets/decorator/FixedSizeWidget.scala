package gui4s.desktop.kit.widgets.decorator

import cats.*
import cats.effect.*
import catnip.syntax.all.given
import gui4s.core.geometry.{Axis, InfinityOr, Rect}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given

import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.linearContainer
import gui4s.core.widget.library.decorator.Decorator

def fixedSizeWidget[
  IO[_] : Sync,
  Event
](
   size : Rect[Float],
   horizontalPlacement : OneElementLinearContainerPlacementStrategy[IO],
   verticalPlacement : OneElementLinearContainerPlacementStrategy[IO]
) : Decorator[DesktopWidget[IO, Event]] =
  gui4s.desktop.widget.library.decorator.fixedSizeWidget(
      Place.withBoundsK(
        _ => size.map(new InfinityOr(_))
      ),
    linearContainer[
      IO,
      Event,
      Id,
    ](
      [A : Order, B] => (value : A) => (f : A => Update[IO, Event, B]) =>
        f(value)
    ),
    Axis.Horizontal,
    horizontalPlacement,
    verticalPlacement
  )
end fixedSizeWidget