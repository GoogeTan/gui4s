package gui4s.desktop.kit.widgets

import catnip.syntax.all.given
import cats.Id
import gui4s.core.geometry.Axis
import gui4s.core.kit.ContainerPlacementError
import gui4s.desktop.kit.effects.{LinearContainerPlacementStrategy, OneElementLinearContainerPlacementStrategy}

def boxWidget[Event](
                      child : DesktopWidget[Event],
                      horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English),
                      verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English)
                    ) : DesktopWidget[Event] =
  linearContainerWidget[Event, Id](
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end boxWidget