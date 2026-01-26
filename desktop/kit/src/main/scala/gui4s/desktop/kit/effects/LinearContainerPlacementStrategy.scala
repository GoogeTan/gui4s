package gui4s.desktop.kit
package effects

import cats._

import gui4s.core.geometry.InfinityOr
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy => GenericOneElementPlacementStrategy}
import gui4s.core.layout.rowcolumn.{PlacementStrategy => GenericPlacementStrategy}

import gui4s.desktop.kit.effects.PlacementEffect.given

type LinearContainerPlacementStrategy[IO[_], Collecton[_]]
    = GenericPlacementStrategy[PlacementEffectC[IO], Float, InfinityOr[Float], Collecton, Float]
type OneElementLinearContainerPlacementStrategy[IO[_]]
    = GenericOneElementPlacementStrategy[PlacementEffectC[IO], Float, InfinityOr[Float], Float]

object LinearContainerPlacementStrategy:
  def Begin[
    IO[_] : Monad,
    Collection[_] : Traverse
  ](gap : Float) : LinearContainerPlacementStrategy[IO, Collection] =
    GenericPlacementStrategy.Begin[PlacementEffect[IO, *], InfinityOr[Float], Collection, Float](gap)
  end Begin

  def Center[
    IO[_] : Monad,
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[IO, Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.Center[PlacementEffect[IO, *], Collection, Float](gap),
      errors.withCenterStrategy
    )
  end Center

  def End[
    IO[_] : Monad,
    Collection[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[IO, Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.End[PlacementEffect[IO, *], Collection, Float](gap),
      errors.withEndStrategy
    )
  end End

  def SpaceBetween[
    IO[_] : Monad,
    Collection[_] : Traverse
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[IO, Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceBetween[PlacementEffect[IO, *], Collection, Float],
      errors.withSpaceBetweenStrategy
    )
  end SpaceBetween

  def SpaceAround[
    IO[_] : Monad,
    Collection[_] : {Applicative, Traverse, SemigroupK}
  ](errors : ContainerPlacementError[Throwable]) : LinearContainerPlacementStrategy[IO, Collection] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceAround[PlacementEffect[IO, *], Collection, Float],
      errors.withSpaceAroundStrategy
    )
  end SpaceAround
end LinearContainerPlacementStrategy
