package gui4s.desktop.kit.cats
package effects

import catnip.{Get, Set}
import cats.*
import cats.effect.IO
import cats.data.EitherT
import gui4s.desktop.kit.effects.OuterPlace as GenericOuterPlace

type OuterPlace[T] = GenericOuterPlace[IO, T]

object OuterPlace:
  given monadInstance: MonadThrow[OuterPlace] =
    GenericOuterPlace.monadInstance
  end monadInstance  

  def liftK: IO ~> OuterPlace =
    GenericOuterPlace.liftK[IO]
  end liftK

  def liftF[Value](value: IO[Value]): OuterPlace[Value] =
    liftK(value)
  end liftF

  def getBounds: Get[OuterPlace, Bounds] =
    GenericOuterPlace.getBounds[IO]
  end getBounds

  def setBounds: Set[OuterPlace, Bounds] =
    GenericOuterPlace.setBounds[IO]
  end setBounds

  def withBounds[T](original: OuterPlace[T], f: Bounds => Bounds): OuterPlace[T] =
    GenericOuterPlace.withBounds(original, f)
  end withBounds

  def raiseError[Value](error: => Throwable): OuterPlace[Value] =
    GenericOuterPlace.raiseError(error)
  end raiseError

  def run(bounds: IO[Bounds]): OuterPlace ~> EitherT[IO, Throwable, *] =
    GenericOuterPlace.run[IO](bounds)
  end run
end OuterPlace
