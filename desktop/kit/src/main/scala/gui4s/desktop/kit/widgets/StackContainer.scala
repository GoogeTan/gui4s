package gui4s.desktop.kit
package widgets

import cats._
import cats.effect._

import gui4s.core.geometry._
import gui4s.core.layout.{PlacementStrategy => GenericPlacementStrategy}

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
    PlacementEffect,
    Rect[Float],
    Bounds,
    Float,
  ](
    getBounds = PlacementEffect.getBounds,
    container = listContainerWidget[Event],
    widgetAsFree = gui4s.desktop.widget.library.widgetAsFree,
  )(
    children,
    GenericPlacementStrategy.Stack(
      Axis.Vertical, verticalPlacement, horizontalPlacement,
    )
  )
end stackContainer
