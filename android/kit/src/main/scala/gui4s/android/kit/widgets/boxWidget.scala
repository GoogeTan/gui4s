package gui4s.android.kit.widgets

import catnip.syntax.all.given
import cats.Id
import cats.effect.IO
import gui4s.core.geometry.Axis
import gui4s.core.kit.ContainerPlacementError
import gui4s.android.kit.effects.{LinearContainerPlacementStrategy, OneElementLinearContainerPlacementStrategy}

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
  linearContainerWidget[Event, Id](
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end boxWidget
