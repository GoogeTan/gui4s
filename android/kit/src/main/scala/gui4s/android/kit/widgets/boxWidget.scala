package gui4s.android.kit.widgets

import catnip.syntax.all.given
import cats.Id
import gui4s.core.geometry.Point3d
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.{PlacementStrategy, Sized}
import gui4s.android.kit.effects.*

def boxWidget[Event](
                      child : AndroidWidget[Event],
                    ) : AndroidWidget[Event] =
  boxWidget(
    child,
    LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English),
    LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English)
  )
end boxWidget

def boxWidget[Event](
                      child : AndroidWidget[Event],
                      horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy,
                      verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy
                    ) : AndroidWidget[Event] =
  val xyPlacement = PlacementStrategy.Zip(horizontalPlacementStrategy, verticalPlacementStrategy)
  oneElementContainerWidget[Event](
    child,
    (measuredChildEffect, bounds) =>
      for
        measuredChild <- measuredChildEffect
        placed <- xyPlacement(measuredChild.size, bounds)
      yield Sized(
        measuredChild.map((_, new Point3d(placed.value, 0f))),
        placed.size
      )
  )
end boxWidget
