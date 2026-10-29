package gui4s.desktop.kit
package effects

import cats.*
import cats.effect.*

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

opaque type PlacementEffect[T] <: AnyRef = (Bounds, Path) => IO[T]

object PlacementEffect:
  inline def apply[T](inline runEffect: (Bounds, Path) => IO[T]): PlacementEffect[T] =
    runEffect

  extension[T](self : PlacementEffect[T])
    inline def map[B](f: T => B): PlacementEffect[B] =
      (bounds, path) => self(bounds, path).map(f)

    inline def flatMap[B](f: T => PlacementEffect[B]): PlacementEffect[B] =
      (bounds, path) =>
        self(bounds, path).flatMap(t => f(t)(bounds, path))

  given syncInstance: Sync[PlacementEffect] = new Sync[PlacementEffect]:
    override def rootCancelScope: cats.effect.kernel.CancelScope =
      Sync[IO].rootCancelScope
    
    override def pure[A](x: A): PlacementEffect[A] =
      PlacementEffect((_, _) => IO.pure(x))

    override def flatMap[A, B](fa: PlacementEffect[A])(f: A => PlacementEffect[B]): PlacementEffect[B] =
      fa.flatMap(f)

    override def map[A, B](fa: PlacementEffect[A])(f: A => B): PlacementEffect[B] =
      fa.map(f)
    end map

    override def tailRecM[A, B](a: A)(f: A => PlacementEffect[Either[A, B]]): PlacementEffect[B] =
      PlacementEffect((bounds, path) =>
        Sync[IO].tailRecM(a)(a0 => f(a0)(bounds, path))
      )

    override def raiseError[A](e: Throwable): PlacementEffect[A] =
      PlacementEffect((_, _) => IO.raiseError(e))

    override def handleErrorWith[A](fa: PlacementEffect[A])(f: Throwable => PlacementEffect[A]): PlacementEffect[A] =
      PlacementEffect((bounds, path) =>
        fa(bounds, path).handleErrorWith(e => f(e)(bounds, path))
      )

    override def canceled: PlacementEffect[Unit] =
      PlacementEffect((_, _) => IO.canceled)

    override def onCancel[A](fa: PlacementEffect[A], fin: PlacementEffect[Unit]): PlacementEffect[A] =
      PlacementEffect((bounds, path) =>
        fa(bounds, path).onCancel(fin(bounds, path))
      )

    override def forceR[A, B](fa: PlacementEffect[A])(fb: PlacementEffect[B]): PlacementEffect[B] =
      PlacementEffect((bounds, path) =>
        fa(bounds, path).forceR(fb(bounds, path))
      )

    override def uncancelable[A](body: Poll[PlacementEffect] => PlacementEffect[A]): PlacementEffect[A] =
      PlacementEffect((bounds, path) =>
        IO.uncancelable { ioPoll =>
          val pePoll = new Poll[PlacementEffect]:
            def apply[X](fa: PlacementEffect[X]): PlacementEffect[X] =
              PlacementEffect((b, p) => ioPoll(fa(b, p)))
          body(pePoll)(bounds, path)
        }
      )

    override def monotonic: PlacementEffect[ scala.concurrent.duration.FiniteDuration ] =
      liftF(IO.monotonic)

    override def realTime: PlacementEffect[ scala.concurrent.duration.FiniteDuration ] =
      liftF(IO.realTime)

    override def suspend[A](hint: Sync.Type)(thunk: => A): PlacementEffect[A] =
      PlacementEffect((_, _) => IO.suspend(hint)(thunk))
  end syncInstance

  given monadThrowInstance: MonadThrow[PlacementEffect] =
    syncInstance
  end monadThrowInstance

  def liftK: IO ~> PlacementEffect =
    new (IO ~> PlacementEffect):
      def apply[A](fa: IO[A]): PlacementEffect[A] =
        PlacementEffect((_, _) => fa)
  end liftK

  def liftF[Value](value: IO[Value]): PlacementEffect[Value] =
    liftK(value)
  end liftF

  def liftSized[Value](value: Sized[Rect[Float], Value]): PlacementEffect[Sized[Rect[Float], Value]] =
    syncInstance.pure(value)
  end liftSized

  def liftFunction[Value](value: Bounds => IO[Value]): PlacementEffect[Value] =
    PlacementEffect((bounds, _) => value(bounds))
  end liftFunction

  def getContext: PlacementEffect[(Bounds, Path)] =
    PlacementEffect((bounds, path) => IO.pure((bounds, path)))
  end getContext

  def getBounds: PlacementEffect[Bounds] =
    PlacementEffect((bounds, _) => IO.pure(bounds))
  end getBounds

  def withBounds[T](original: PlacementEffect[T], f: Bounds => Bounds): PlacementEffect[T] =
    PlacementEffect((bounds, path) => original(f(bounds), path))
  end withBounds

  def withBoundsK(f: Bounds => Bounds): PlacementEffect ~> PlacementEffect =
    new (PlacementEffect ~> PlacementEffect):
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        withBounds(original, f)
  end withBoundsK

  def withBoundsKF(f: Bounds => PlacementEffect[Bounds]): PlacementEffect ~> PlacementEffect =
    new (PlacementEffect ~> PlacementEffect):
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        for
          bounds <- getBounds
          newBounds <- f(bounds)
          res <- withBounds(original, _ => newBounds)
        yield res
  end withBoundsKF

  def raiseError[Error, Value](error: => Error)(using ME: MonadError[IO, Error]): PlacementEffect[Value] =
    liftF(ME.raiseError(error))
  end raiseError

  def run(path: Path, bounds: IO[Bounds]): PlacementEffect ~> IO =
    new (PlacementEffect ~> IO):
      def apply[A](fa: PlacementEffect[A]): IO[A] =
        bounds.flatMap(b => fa(b, path))
  end run

  def addNameToPath(name: String): PlacementEffect ~> PlacementEffect =
    new (PlacementEffect ~> PlacementEffect):
      def apply[A](fa: PlacementEffect[A]): PlacementEffect[A] =
        PlacementEffect((bounds, path) => fa(bounds, path / name))
  end addNameToPath

  def currentPath: PlacementEffect[Path] =
    PlacementEffect((_, path) => IO.pure(path))
  end currentPath
end PlacementEffect