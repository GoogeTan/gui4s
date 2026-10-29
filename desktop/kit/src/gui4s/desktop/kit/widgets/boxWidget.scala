package gui4s.desktop.kit.widgets

import catnip.syntax.all.given
import cats.Id

import gui4s.core.geometry.Point3d
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.PlacementStrategy
import gui4s.core.layout.Sized

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.PlacementEffect.{flatMap, map}

def boxWidget[Event](
                      child : DesktopWidget[Event],
                      horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English),
                      verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English)
                    ) : DesktopWidget[Event] =
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