package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.given
import cats.*
import cats.arrow.FunctionK
import cats.effect.*

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Rect
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.linearContainerWidget

def fixedSizeWidget[
  Event
](
   size : Rect[Float],
   horizontalPlacement : OneElementLinearContainerPlacementStrategy,
   verticalPlacement : OneElementLinearContainerPlacementStrategy
) : Decorator[DesktopWidget[Event]] =
  gui4s.desktop.widget.library.decorator.fixedSizeWidget(
    withPreferredSize = FunctionK.lift(
      [T] => (value : Place[T]) =>
        Place
          .withBoundsK(_ => size.map(new InfinityOr(_)))(value)
          .map:
            case Sized(widget, _) =>
              Sized(widget, size)
    ),
    linearContainer = linearContainerWidget[Event, Id],
    mainAxis = Axis.Horizontal,
    mainAxisStrategy = horizontalPlacement,
    crossAxisStrategy = verticalPlacement
  )
end fixedSizeWidget

extension[Event](value : DesktopWidget[Event])
  def fixedSize(
    size: Rect[Float],
    horizontalPlacement: OneElementLinearContainerPlacementStrategy = OneElementPlacementStrategy.Begin,
    verticalPlacement: OneElementLinearContainerPlacementStrategy = OneElementPlacementStrategy.Begin
  ): DesktopWidget[Event] =
    fixedSizeWidget(size, horizontalPlacement, verticalPlacement)(value)
  end fixedSize
end extension