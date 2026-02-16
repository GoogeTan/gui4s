package gui4s.desktop.kit
package effects

import cats._
import cats.effect.*

import gui4s.core.geometry.InfinityOr
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy => GenericOneElementPlacementStrategy}
import gui4s.core.layout.rowcolumn.{PlacementStrategy => GenericPlacementStrategy}

import gui4s.desktop.kit.effects.PlacementEffect.given

type LinearContainerPlacementStrategy[Collection[_]]
    = GenericPlacementStrategy[PlacementEffectC[IO], Float, InfinityOr[Float], Collection, Float]
type OneElementLinearContainerPlacementStrategy
    = GenericOneElementPlacementStrategy[PlacementEffectC[IO], Float, InfinityOr[Float], Float]

object LinearContainerPlacementStrategy:
  def Begin[
    Collection[_] : Traverse
  ](gap : Float) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.Begin[PlacementEffect[IO, *], InfinityOr[Float], Collection, Float](gap)
  end Begin

  def Center[
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity[PlacementEffectC[IO], Float, Collection, Throwable](
      GenericPlacementStrategy.Center[PlacementEffect[IO, *], Collection, Float](gap),
      errors.withCenterStrategy
    )
  end Center

  def End[
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.End[PlacementEffect[IO, *], Collection, Float](gap),
      errors.withEndStrategy
    )
  end End

  def SpaceBetween[
    Collection[_] : Traverse
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceBetween[PlacementEffect[IO, *], Collection, Float],
      errors.withSpaceBetweenStrategy
    )
  end SpaceBetween

  def SpaceAround[
    Collection[_] : {Applicative, Traverse, SemigroupK}
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceAround[PlacementEffect[IO, *], Collection, Float],
      errors.withSpaceAroundStrategy
    )
  end SpaceAround
end LinearContainerPlacementStrategy
