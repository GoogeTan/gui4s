package gui4s.android.kit.widgets

import cats.*
import cats.effect.*
import gui4s.core.geometry.*
import gui4s.core.layout.PlacementStrategy as GenericPlacementStrategy
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

import cats.effect.IO
import cats.*
import cats.effect.*
import gui4s.core.geometry.*
import gui4s.core.layout.PlacementStrategy as GenericPlacementStrategy
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

def stackContainer[Event](
                           children : List[AndroidWidget[Event]],
                           verticalPlacement : OneElementLinearContainerPlacementStrategy,
                           horizontalPlacement : OneElementLinearContainerPlacementStrategy
) : AndroidWidget[Event] =
  given Ordering[Rect[Float]] = Ordering.by(point => math.max(point.width, point.height))
  gui4s.core.widget.library.stackContainer[
    AndroidPlacedWidget[Event],
    PlacementEffectC,
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
