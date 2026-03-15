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

type PlacementEffect[T] = GenericPlacementEffect[IO, Bounds, T]

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

  def getBounds: Get[PlacementEffect, Bounds] =
    GenericPlacementEffect.getBounds
  end getBounds

  def setBounds: Set[PlacementEffect, Bounds] =
    GenericPlacementEffect.setBounds
  end setBounds

  def withBounds[T](original: PlacementEffect[T], f: Bounds => Bounds): PlacementEffect[T] =
    GenericPlacementEffect.withBounds(original, f)
  end withBounds

  def withBoundsK(f: Bounds => Bounds): PlacementEffect ~> PlacementEffect =
    new (PlacementEffect ~> PlacementEffect) {
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        withBounds(original, f)
    }
  end withBoundsK

  def raiseError[Error, Value](error: => Error)(using ME : MonadError[IO, Error]): PlacementEffect[Value] =
    GenericPlacementEffect.liftF(ME.raiseError(error))
  end raiseError

  def run(path : Path, bounds: IO[Bounds]): PlacementEffect ~> IO =
    GenericPlacementEffect.run(path, bounds)
  end run

  def addNameToPath(name: String): PlacementEffect ~> PlacementEffect =
    GenericPlacementEffect.addNameToPath(name)
  end addNameToPath

  def currentPath: PlacementEffect[Path]  =
    GenericPlacementEffect.currentPath
  end currentPath
end PlacementEffect
