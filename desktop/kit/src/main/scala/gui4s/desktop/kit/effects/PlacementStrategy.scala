package gui4s.desktop.kit
package effects

import cats.*
import gui4s.core.geometry.InfinityOr
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy as GenericOneElementPlacementStrategy, PlacementStrategy as GenericPlacementStrategy}
import gui4s.desktop.kit.effects.OuterPlace.given

type PlacementStrategy[IO[_], Bounds, Container[_], Point] = GenericPlacementStrategy[OuterPlaceC[IO], Bounds, Container, Point]
type OneElementPlacementStrategy[IO[_], Bounds, Point] = GenericOneElementPlacementStrategy[OuterPlaceC[IO], Bounds, Point]

object PlacementStrategy:
  def Begin[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float) : PlacementStrategy[IO, InfinityOr[Float], Container, Float] =
    GenericPlacementStrategy.Begin[OuterPlace[IO, *], InfinityOr[Float], Container, Float](gap)
  end Begin

  def Center[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, InfinityOr[Float], Container, Float] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.Center[OuterPlace[IO, *], Container, Float](gap),
      errors.withCenterStrategy
    )
  end Center

  def End[
    IO[_] : Monad,
    Container[_] : Traverse
  ](gap : Float, errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, InfinityOr[Float], Container, Float] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.End[OuterPlace[IO, *], Container, Float](gap),
      errors.withEndStrategy
    )
  end End

  def SpaceBetween[
    IO[_] : Monad,
    Container[_] : Traverse
  ](errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, InfinityOr[Float], Container, Float] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceBetween[OuterPlace[IO, *], Container, Float],
      errors.withSpaceBetweenStrategy
    )
  end SpaceBetween

  def SpaceAround[
    IO[_] : Monad,
    Container[_] : {Applicative, Traverse, SemigroupK}
  ](errors : ContainerPlacementError[Throwable]) : PlacementStrategy[IO, InfinityOr[Float], Container, Float] =
    GenericPlacementStrategy.ErrorIfInfinity(
      GenericPlacementStrategy.SpaceAround[OuterPlace[IO, *], Container, Float],
      errors.withSpaceAroundStrategy
    )
  end SpaceAround
end PlacementStrategy
