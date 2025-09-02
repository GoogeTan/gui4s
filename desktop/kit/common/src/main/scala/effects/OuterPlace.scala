package gui4s.desktop.kit
package common.effects

import catnip.{Get, Set}
import cats.*
import cats.data.EitherT
import gui4s.core.kit.effects.OuterPlace as GenericOuterPlace

type OuterPlace[IO[_], T] = GenericOuterPlace[IO, Bounds, Throwable, T]
type OuterPlaceT[IO[_]] = [Value] =>> OuterPlace[IO, Value]

object OuterPlace:
  given monadInstance[IO[_] : Monad]: MonadThrow[OuterPlaceT[IO]] =
    summon

  def liftK[IO[_] : Monad]: IO ~> OuterPlaceT[IO] =
    GenericOuterPlace.liftK
  end liftK

  def liftF[IO[_] : Monad, Value](value: IO[Value]): OuterPlace[IO, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad]: Get[OuterPlaceT[IO], Bounds] =
    GenericOuterPlace.getBounds
  end getBounds

  def setBounds[IO[_] : Monad]: Set[OuterPlaceT[IO], Bounds] =
    GenericOuterPlace.setBounds
  end setBounds

  def withBounds[IO[_] : Monad, T](original: OuterPlace[IO, T], f: Bounds => Bounds): OuterPlace[IO, T] =
    GenericOuterPlace.withBounds(original, f)
  end withBounds

  def raiseError[IO[_] : Monad, Value](error: => Throwable): OuterPlace[IO, Value] =
    GenericOuterPlace.raiseError(error)
  end raiseError

  def run[IO[_] : Monad](bounds: IO[Bounds]): OuterPlaceT[IO] ~> EitherT[IO, Throwable, *] =
    GenericOuterPlace.run[IO, Bounds, Throwable](bounds)
  end run
end OuterPlace
