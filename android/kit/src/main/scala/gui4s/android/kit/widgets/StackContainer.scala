package gui4s.android.kit.widgets

import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.PlacementStrategy as GenericPlacementStrategy
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

def stackContainer[IO[_] : Sync as S, Event](
                                              children : List[AndroidWidget[IO, Event]],
                                              verticalPlacement : OneElementLinearContainerPlacementStrategy[IO],
                                              horizontalPlacement : OneElementLinearContainerPlacementStrategy[IO]
) : AndroidWidget[IO, Event] =
  given Ordering[Point2d[Float]] = Ordering.by(point => math.max(point.x, point.y))
  gui4s.core.widget.library.stackContainer[
    AndroidPlacedWidget[IO, Event],
    OuterPlaceC[IO],
    Bounds,
    Float,
  ](
    getBounds = OuterPlace.getBounds[IO],
    container = container[IO, List, Event](traverseOrdered)
  )(
    children,
    GenericPlacementStrategy.PlaceIndependently(
      GenericPlacementStrategy.Zip(Axis.Vertical, verticalPlacement, horizontalPlacement),
      Point2d(0f, 0f)
    )
  )
end stackContainer
