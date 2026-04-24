package gui4s.desktop.kit
package effects

import scala.reflect.Typeable

import catnip.MapKCache
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.Functor
import cats.MonadError
import cats.effect.*
import cats.~>

import gui4s.core.widget.Path

import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.effects.Situated.given
import gui4s.desktop.skija.shaper.Shaper

type Place[T] = PlacementEffect[Situated[T]]

object Place:
  def run(path : Path, bounds : IO[Bounds]) : Place[*] ~> IO =
    new ~>[Place, IO]:
      override def apply[A](fa : Place[A]) : IO[A] =
        PlacementEffect.run(path, bounds)(fa.map(_.value))
      end apply
    end new
  end run

  def withBoundsK(f : Bounds => Bounds) : Place ~> Place =
    new ~>[Place, Place]:
      override def apply[A](fa : Place[A]) : Place[A] =
        PlacementEffect.withBoundsK(f)(fa)
      end apply
    end new
  end withBoundsK
  
  def sizeText(
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place] =
    sizeTextFFI[PlacementEffect](
      PlacementEffect.getBounds.map(_.width.value),
      shaper,
      MapKCache(cache, PlacementEffect.liftK),
    )
  end sizeText

  def typecheck[TypeToCheck : Typeable](error : (Any, Path) => Throwable) : [Res] => (Any, TypeToCheck => Option[Place[Res]]) => Option[Place[Res]] =
    [Res] => (valueToCheck : Any, callback : TypeToCheck => Option[Place[Res]]) =>
      valueToCheck match
        case v : TypeToCheck =>
          callback(v)
        case valueFound =>
          Some(
            PlacementEffect.currentPath.flatMap(path =>
              PlacementEffect.raiseError(error(valueFound, path))
            )
          )
      end match
  end typecheck

  given functorInstance : Functor[Place] =
    nestedFunctorsAreFunctors[PlacementEffect, Situated]
  end functorInstance

  def addNameToPath(name : String) : Place ~> Place =
    new (Place ~> Place):
      override def apply[A](fa: Place[A]): Place[A] =
        PlacementEffect.addNameToPath(name)(fa)
      end apply
    end new
  end addNameToPath

  def raiseError[Error, Value](error: => Error)(using ME: MonadError[IO, Error]): Place[Value] =
    PlacementEffect.liftF(ME.raiseError(error))
  end raiseError

  def raiseError[Error, Value](error: Path => Error)(using MonadError[IO, Error]): Place[Value] =
    PlacementEffect.currentPath.map(error(_)).flatMap(raiseError)
  end raiseError
end Place
