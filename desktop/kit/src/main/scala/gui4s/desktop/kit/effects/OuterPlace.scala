package gui4s.desktop.kit
package effects

import catnip.{Get, Set}
import cats.*
import cats.data.EitherT
import gui4s.core.kit.effects.OuterPlace as GenericOuterPlace
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

type OuterPlace[IO[_], T] = GenericOuterPlace[IO, Bounds, Throwable, T]
type OuterPlaceC[IO[_]] = [Value] =>> OuterPlace[IO, Value]

object OuterPlace:
  given monadInstance[IO[_] : Monad]: MonadThrow[OuterPlaceC[IO]] =
    GenericOuterPlace.monadInstance
  end monadInstance

  def liftK[IO[_] : Monad]: IO ~> OuterPlaceC[IO] =
    GenericOuterPlace.liftK
  end liftK

  def liftF[IO[_] : Monad, Value](value: IO[Value]): OuterPlace[IO, Value] =
    liftK(value)
  end liftF

  def liftSized[IO[_] : Monad as A, Value](value: Sized[Float, Value]): OuterPlace[IO, Sized[Float, Value]] =
    monadInstance.pure(value)
  end liftSized

  def liftFunction[IO[_] : Monad, Value](value: Bounds => IO[Value]): OuterPlace[IO, Value] =
    getBounds.flatMap(
      bounds => liftF(value(bounds))
    )
  end liftFunction

  def getBounds[IO[_] : Monad]: Get[OuterPlaceC[IO], Bounds] =
    GenericOuterPlace.getBounds
  end getBounds

  def setBounds[IO[_] : Monad]: Set[OuterPlaceC[IO], Bounds] =
    GenericOuterPlace.setBounds
  end setBounds

  def withBounds[IO[_] : Monad, T](original: OuterPlace[IO, T], f: Bounds => Bounds): OuterPlace[IO, T] =
    GenericOuterPlace.withBounds(original, f)
  end withBounds

  def withBoundsK[IO[_] : Monad](f: Bounds => Bounds): OuterPlace[IO, *] ~> OuterPlace[IO, *] =
    new (OuterPlace[IO, *] ~> OuterPlace[IO, *]) {
      def apply[A](original: OuterPlace[IO, A]): OuterPlace[IO, A] =
        withBounds(original, f)
    }
  end withBoundsK

  def raiseError[IO[_] : Monad, Value](error: => Throwable): OuterPlace[IO, Value] =
    GenericOuterPlace.raiseError(error)
  end raiseError

  def run[IO[_] : Monad](path : Path, bounds: IO[Bounds]): OuterPlaceC[IO] ~> EitherT[IO, Throwable, *] =
    GenericOuterPlace.run[IO, Bounds, Throwable](path, bounds)
  end run

  def addNameToPath[IO[_] : Monad](name: String): OuterPlaceC[IO] ~> OuterPlaceC[IO] =
    GenericOuterPlace.addNameToPath(name)
  end addNameToPath

  def currentPath[IO[_] : Monad]: OuterPlace[IO, Path]  =
    GenericOuterPlace.currentPath
  end currentPath
end OuterPlace
