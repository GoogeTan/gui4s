package gui4s.desktop.kit
package effects

import cats.*
import gui4s.core.geometry.InfinityOr
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy as GenericOneElementPlacementStrategy, PlacementStrategy as GenericPlacementStrategy}
import gui4s.desktop.kit.effects.OuterPlace.given

type PlacementStrategy[IO[_], Container[_]] 
    = GenericPlacementStrategy[OuterPlaceC[IO], InfinityOr[Float], Container, Float]
type OneElementPlacementStrategy[IO[_]] 
    = GenericOneElementPlacementStrategy[OuterPlaceC[IO], InfinityOr[Float], Float]

object PlacementStrategy:
  def Begin[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float) : PlacementStrategy[IO, Container] =
    GenericPlacementStrategy.Begin[OuterPlace[IO, *], InfinityOr[Float], Container, Float](gap)
  end Begin

  def Center[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, Container] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.Center[OuterPlace[IO, *], Container, Float](gap),
      errors.withCenterStrategy
    )
  end Center

  def End[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, Container] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.End[OuterPlace[IO, *], Container, Float](gap),
      errors.withEndStrategy
    )
  end End

  def SpaceBetween[
    IO[_] : Monad,
    Container[_] : Traverse
  ](errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, Container] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceBetween[OuterPlace[IO, *], Container, Float],
      errors.withSpaceBetweenStrategy
    )
  end SpaceBetween

  def SpaceAround[
    IO[_] : Monad,
    Container[_] : {Applicative, Traverse, SemigroupK}
  ](errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, Container] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceAround[OuterPlace[IO, *], Container, Float],
      errors.withSpaceAroundStrategy
    )
  end SpaceAround
end PlacementStrategy
