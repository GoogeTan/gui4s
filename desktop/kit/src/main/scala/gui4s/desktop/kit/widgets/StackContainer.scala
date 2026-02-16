package gui4s.desktop.kit
package widgets

import catnip.syntax.all._
import cats._
import cats.effect._

import gui4s.core.geometry._
import gui4s.core.layout.rowcolumn.{PlacementStrategy => GenericPlacementStrategy}

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget

def stackContainer[Event](
                           children : List[DesktopWidget[Event]],
                           verticalPlacement : OneElementLinearContainerPlacementStrategy,
                           horizontalPlacement : OneElementLinearContainerPlacementStrategy
) : DesktopWidget[Event] =
  given Ordering[Rect[Float]] = Ordering.by(point => math.max(point.width, point.height))
  gui4s.core.widget.library.stackContainer[
    DesktopPlacedWidget[Event],
    PlacementEffectC[IO],
    Bounds,
    Float,
  ](
    getBounds = PlacementEffect.getBounds[IO],
    container = containerWidget[List, Event](traverseOrdered)
  )(
    children,
    GenericPlacementStrategy.Stack(
      Axis.Vertical, verticalPlacement, horizontalPlacement,
    )
  )
end stackContainer
