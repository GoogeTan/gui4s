package gui4s.desktop.kit
package effects

import scala.reflect.Typeable

import catnip.MapKCache
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.Functor
import cats.Monad
import cats.MonadThrow
import cats.data.EitherT
import cats.effect.Sync
import cats.~>

import gui4s.core.kit.effects.{Place => GenericPlace}
import gui4s.core.widget.Path

import gui4s.desktop.kit.effects.Situated.given
import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.skija.shaper.Shaper

type Place[IO[_], T] = GenericPlace[IO, Bounds, Float, Throwable, T]
type PlaceC[IO[_]] = [Value] =>> Place[IO, Value]

object Place:
  def run[IO[_] : Monad](path : Path, bounds : IO[Bounds]) : Place[IO, *] ~> EitherT[IO, Throwable, *] =
    new ~>[Place[IO, *], EitherT[IO, Throwable, *]]:
      override def apply[A](fa : Place[IO, A]) : EitherT[IO, Throwable, A] =
        PlacementEffect.run(path, bounds)(fa.map(_.value))
      end apply
    end new
  end run

  def withBoundsK[IO[_] : Sync](f : Bounds => Bounds) : Place[IO, *] ~> Place[IO, *] =
    new ~>[Place[IO, *], Place[IO, *]]:
      override def apply[A](fa : Place[IO, A]) : Place[IO, A] =
        PlacementEffect.withBoundsK(f)(fa)
      end apply
    end new
  end withBoundsK
  
  def sizeText[IO[_] : Sync](
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place[IO, *]] =
    sizeTextFFI[PlacementEffect[IO, *]](
      PlacementEffect.getBounds.map(_.width.value),
      shaper,
      MapKCache(cache, PlacementEffect.liftK),
    )
  end sizeText

  def typecheck[IO[_] : MonadThrow, TypeToCheck : Typeable](error : (Any, Path) => Throwable) : [Res] => (Any, Path, TypeToCheck => Place[IO, Res]) => Place[IO, Res] =
    GenericPlace.typecheck[PlacementEffect[IO, *], Situated, Throwable, TypeToCheck](error)
  end typecheck

  given functorInstance[IO[_] : Monad] : Functor[Place[IO, *]] =
    nestedFunctorsAreFunctors[PlacementEffect[IO, *], Situated]
  end functorInstance

  def addNameToPath[IO[_] : Monad](name : String) : Place[IO, *] ~> Place[IO, *] =
    GenericPlace.addNameToPath(name)
  end addNameToPath
end Place
