package gui4s.desktop.kit
package effects

import cats._

import gui4s.core.geometry.InfinityOr
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.{OneElementPlacementStrategy => GenericOneElementPlacementStrategy}
import gui4s.core.layout.{PlacementStrategy => GenericPlacementStrategy}

import gui4s.desktop.kit.effects.PlacementEffect.given

type LinearContainerPlacementStrategy[Collection[_]]
    = GenericPlacementStrategy[PlacementEffect, Float, Float, InfinityOr[Float], Collection, Float]
type OneElementLinearContainerPlacementStrategy
    = GenericOneElementPlacementStrategy[PlacementEffect, Float, Float, InfinityOr[Float], Float]

object LinearContainerPlacementStrategy:
  def Begin[
    Collection[_] : Traverse
  ](gap : Float) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.Begin[PlacementEffect, InfinityOr[Float], Collection, Float](gap)
  end Begin

  def Center[
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity[PlacementEffect, Float, Collection, Throwable](
      GenericPlacementStrategy.Center[PlacementEffect, Collection, Float](gap),
      errors.withCenterStrategy
    )
  end Center

  def End[
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.End[PlacementEffect, Collection, Float](gap),
      errors.withEndStrategy
    )
  end End

  def SpaceBetween[
    Collection[_] : Traverse
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceBetween[PlacementEffect, Collection, Float],
      errors.withSpaceBetweenStrategy
    )
  end SpaceBetween

  def SpaceAround[
    Collection[_] : {Applicative, Traverse, SemigroupK}
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceAround[PlacementEffect, Collection, Float],
      errors.withSpaceAroundStrategy
    )
  end SpaceAround
end LinearContainerPlacementStrategy
