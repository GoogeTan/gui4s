package gui4s.desktop.kit
package effects

import catnip.Get
import catnip.Set
import cats.*
import cats.data.EitherT
import gui4s.core.kit.effects.PlacementEffect as GenericPlacementEffect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

import scala.util.NotGiven

type PlacementEffect[IO[_], T] = GenericPlacementEffect[IO, Bounds, T]
type PlacementEffectC[IO[_]] = [Value] =>> PlacementEffect[IO, Value]

object PlacementEffect:
  given monadThrowInstance[IO[_] : MonadThrow]: MonadThrow[PlacementEffectC[IO]] =
    GenericPlacementEffect.monadThrowInstance
  end monadThrowInstance

  given monadInstance[IO[_] : Monad](using NotGiven[MonadThrow[IO]]): Monad[PlacementEffectC[IO]] =
    GenericPlacementEffect.monadInstance
  end monadInstance

  def liftK[IO[_] : Monad]: IO ~> PlacementEffectC[IO] =
    GenericPlacementEffect.liftK
  end liftK

  def liftF[IO[_] : Monad, Value](value: IO[Value]): PlacementEffect[IO, Value] =
    liftK(value)
  end liftF

  def liftSized[IO[_] : Monad as A, Value](value: Sized[Float, Value]): PlacementEffect[IO, Sized[Float, Value]] =
    monadInstance.pure(value)
  end liftSized

  def liftFunction[IO[_] : Monad, Value](value: Bounds => IO[Value]): PlacementEffect[IO, Value] =
    getBounds.flatMap(
      bounds => liftF(value(bounds))
    )
  end liftFunction

  def getBounds[IO[_] : Monad]: Get[PlacementEffectC[IO], Bounds] =
    GenericPlacementEffect.getBounds
  end getBounds

  def setBounds[IO[_] : Monad]: Set[PlacementEffectC[IO], Bounds] =
    GenericPlacementEffect.setBounds
  end setBounds

  def withBounds[IO[_] : Monad, T](original: PlacementEffect[IO, T], f: Bounds => Bounds): PlacementEffect[IO, T] =
    GenericPlacementEffect.withBounds(original, f)
  end withBounds

  def withBoundsK[IO[_] : Monad](f: Bounds => Bounds): PlacementEffect[IO, *] ~> PlacementEffect[IO, *] =
    new (PlacementEffect[IO, *] ~> PlacementEffect[IO, *]) {
      def apply[A](original: PlacementEffect[IO, A]): PlacementEffect[IO, A] =
        withBounds(original, f)
    }
  end withBoundsK

  def raiseError[IO[_], Error, Value](error: => Error)(using ME : MonadError[IO, Error]): PlacementEffect[IO, Value] =
    GenericPlacementEffect.liftF(ME.raiseError(error))
  end raiseError

  def run[IO[_] : Monad](path : Path, bounds: IO[Bounds]): PlacementEffectC[IO] ~> IO =
    GenericPlacementEffect.run(path, bounds)
  end run

  def addNameToPath[IO[_] : Monad](name: String): PlacementEffectC[IO] ~> PlacementEffectC[IO] =
    GenericPlacementEffect.addNameToPath(name)
  end addNameToPath

  def currentPath[IO[_] : Monad]: PlacementEffect[IO, Path]  =
    GenericPlacementEffect.currentPath
  end currentPath
end PlacementEffect
