package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.*
import gui4s.core.geometry.*
import gui4s.core.layout.PlacementStrategy as GenericPlacementStrategy
import gui4s.desktop.kit.effects.*
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
    InfinityOr[Float],
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
