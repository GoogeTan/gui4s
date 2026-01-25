package gui4s.android.kit.effects

import catnip.{Get, Set}
import gui4s.core.kit.effects.PlacementEffect as GenericPlacementEffect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

type PlacementEffect[IO[_], T] = GenericPlacementEffect[IO, AndroidConfiguration[Bounds], Throwable, T]
type PlacementEffectC[IO[_]] = [Value] =>> PlacementEffect[IO, Value]

object PlacementEffect:
  given monadInstance[IO[_] : Monad]: MonadThrow[PlacementEffectC[IO]] =
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
    GenericPlacementEffect.getBounds.map(_.bounds)
  end getBounds

  def setBounds[IO[_] : Monad]: Set[PlacementEffectC[IO], Bounds] =
    newBounds =>
      GenericPlacementEffect
        .getBounds[IO, AndroidConfiguration[Bounds], Throwable]
        .flatMap(configuration =>
          GenericPlacementEffect.setBounds[IO, AndroidConfiguration[Bounds], Throwable](
            configuration.withBounds(newBounds)
          )
        )
  end setBounds

  def withBounds[IO[_] : Monad, T](original: PlacementEffect[IO, T], f: Bounds => Bounds): PlacementEffect[IO, T] =
    GenericPlacementEffect.withBounds(original, _.withTransformedBounds(f))
  end withBounds

  def withBoundsK[IO[_] : Monad](f: Bounds => Bounds): PlacementEffect[IO, *] ~> PlacementEffect[IO, *] =
    new (PlacementEffect[IO, *] ~> PlacementEffect[IO, *]) {
      def apply[A](original: PlacementEffect[IO, A]): PlacementEffect[IO, A] =
        withBounds(original, f)
    }
  end withBoundsK

  def raiseError[IO[_] : Monad, Value](error: => Throwable): PlacementEffect[IO, Value] =
    GenericPlacementEffect.raiseError(error)
  end raiseError

  def run[IO[_] : Monad](path : Path, bounds: IO[AndroidConfiguration[Bounds]]): PlacementEffectC[IO] ~> EitherT[IO, Throwable, *] =
    GenericPlacementEffect.run[IO, AndroidConfiguration[Bounds], Throwable](path, bounds)
  end run

  def addNameToPath[IO[_] : Monad](name: String): PlacementEffectC[IO] ~> PlacementEffectC[IO] =
    GenericPlacementEffect.addNameToPath(name)
  end addNameToPath

  def currentPath[IO[_] : Monad]: PlacementEffect[IO, Path]  =
    GenericPlacementEffect.currentPath
  end currentPath
end PlacementEffect
