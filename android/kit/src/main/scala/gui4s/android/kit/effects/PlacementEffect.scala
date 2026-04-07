package gui4s.android.kit
package effects

import catnip.{Get, Set}
import cats.*
import gui4s.core.kit.effects.PlacementEffect as GenericPlacementEffect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.geometry.Rect

import scala.util.NotGiven

import cats.effect.IO
import catnip.{Get, Set}
import cats.*
import gui4s.core.kit.effects.PlacementEffect as GenericPlacementEffect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

import scala.util.NotGiven

type PlacementEffect[T] = GenericPlacementEffect[IO, AndroidConfiguration[Bounds], T]

object PlacementEffect:
  given monadThrowInstance : MonadThrow[PlacementEffect] =
    GenericPlacementEffect.monadThrowInstance
  end monadThrowInstance

  def liftK : IO ~> PlacementEffect =
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

  def getBounds : Get[PlacementEffect, Bounds] =
    GenericPlacementEffect.getBounds[IO, AndroidConfiguration[Bounds]].map(_.bounds)
  end getBounds

  def setBounds : Set[PlacementEffect, Bounds] =
    newBounds =>
      GenericPlacementEffect
        .getBounds[IO, AndroidConfiguration[Bounds]]
        .flatMap(configuration =>
          GenericPlacementEffect.setBounds[IO, AndroidConfiguration[Bounds]](
            configuration.withBounds(newBounds)
          )
        )
  end setBounds

  def withBounds[T](original: PlacementEffect[T], f: Bounds => Bounds): PlacementEffect[T] =
    GenericPlacementEffect.withBounds(original, _.withTransformedBounds(f))
  end withBounds

  def withBoundsK(f: Bounds => Bounds): PlacementEffect[*] ~> PlacementEffect[*] =
    new (PlacementEffect[*] ~> PlacementEffect[*]) {
      def apply[A](original: PlacementEffect[A]): PlacementEffect[A] =
        withBounds(original, f)
    }
  end withBoundsK

  def raiseError[Error, Value](error: => Error)(using ME : MonadError[IO, Error]): PlacementEffect[Value] =
    GenericPlacementEffect.liftF(ME.raiseError(error))
  end raiseError

  def run(path : Path, bounds: IO[AndroidConfiguration[Bounds]]): PlacementEffect ~> IO =
    GenericPlacementEffect.run[IO, AndroidConfiguration[Bounds]](path, bounds)
  end run

  def addNameToPath(name: String): PlacementEffect ~> PlacementEffect =
    GenericPlacementEffect.addNameToPath(name)
  end addNameToPath

  def currentPath : PlacementEffect[Path]  =
    GenericPlacementEffect.currentPath
  end currentPath
end PlacementEffect
