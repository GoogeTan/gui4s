package gui4s.desktop.kit
package effects

import catnip.Get
import catnip.Set
import cats._
import cats.effect._

import gui4s.core.geometry.Rect
import gui4s.core.kit.effects.{PlacementEffect => GenericPlacementEffect}
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

type PlacementEffect[T] = GenericPlacementEffect[IO, (Bounds, Path), T]

object PlacementEffect:
  given monadThrowInstance: MonadThrow[PlacementEffect] =
    GenericPlacementEffect.monadThrowInstance
  end monadThrowInstance

  def liftK: IO ~> PlacementEffect =
    GenericPlacementEffect.liftK
  end liftK

  def liftF[Value](value: IO[Value]): PlacementEffect[Value] =
    liftK(value)
  end liftF

  def liftSized[Value](value: Sized[Rect[Float], Value]): PlacementEffect[Sized[Rect[Float], Value]] =
    monadThrowInstance.pure(value)
  end liftSized

  def liftFunction[Value](value: Bounds => IO[Value]): PlacementEffect[Value] =
    getBounds.flatMap(
      bounds => liftF(value(bounds))
    )
  end liftFunction

  def getContext : PlacementEffect[(Bounds, Path)] =
    GenericPlacementEffect.getContext
  end getContext

  def getBounds: Get[PlacementEffect, Bounds] =
    getContext.map(_._1)
  end getBounds

  def withBounds[T](original: PlacementEffect[T], f: Bounds => Bounds): PlacementEffect[T] =
    GenericPlacementEffect.withContext(original, (bounds, path) => (f(bounds), path))
  end withBounds

  def withBoundsK(f: Bounds => Bounds): PlacementEffect ~> PlacementEffect =
    new (PlacementEffect ~> PlacementEffect) {
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        withBounds(original, f)
    }
  end withBoundsK

  def withBoundsKF(f: Bounds => PlacementEffect[Bounds]): PlacementEffect ~> PlacementEffect =
    new(PlacementEffect ~> PlacementEffect) {
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        for
          bounds <- getBounds
          newBounds <- f(bounds)
          res <- withBounds(original, _ => newBounds)
        yield res
    }
  end withBoundsKF

  def raiseError[Error, Value](error: => Error)(using ME : MonadError[IO, Error]): PlacementEffect[Value] =
    GenericPlacementEffect.liftF(ME.raiseError(error))
  end raiseError

  def run(path : Path, bounds: IO[Bounds]): PlacementEffect ~> IO =
    GenericPlacementEffect.run(bounds.map((_, path)))
  end run

  def addNameToPath(name: String): PlacementEffect ~> PlacementEffect =
    GenericPlacementEffect.withContextK((bounds, path) => (bounds, path / name))
  end addNameToPath

  def currentPath: PlacementEffect[Path] =
    getContext.map(_._2)
  end currentPath
end PlacementEffect
