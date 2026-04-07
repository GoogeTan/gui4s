package gui4s.android.kit
package effects

import scala.reflect.Typeable
import catnip.MapKCache
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.{Functor, Monad, MonadError, MonadThrow, ~>}
import cats.effect.Sync
import gui4s.core.kit.effects.{Place as GenericPlace, PlacementEffect as GenericPlacementEffect}
import gui4s.core.widget.Path
import gui4s.android.kit.effects.PlacementEffect.given
import gui4s.android.kit.effects.Situated.given
import org.jetbrains.skia.shaper.Shaper

import gui4s.core.geometry.*
import cats.effect.IO
import scala.reflect.Typeable
import catnip.MapKCache
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.{Functor, Monad, MonadError, MonadThrow, ~>}
import cats.effect.Sync
import gui4s.core.kit.effects.{Place as GenericPlace, PlacementEffect as GenericPlacementEffect}
import gui4s.core.widget.Path
import gui4s.android.kit.effects.PlacementEffect.given
import gui4s.android.kit.effects.Situated.given
import org.jetbrains.skia.shaper.Shaper

type Place[T] = GenericPlace[IO, AndroidConfiguration[Bounds], Rect[Float], T]

object Place:
  def run(path : Path,  bounds : IO[AndroidConfiguration[Bounds]]) : Place ~> IO =
    new ~>[Place[*], IO]:
      override def apply[A](fa : Place[A]) : IO[A] =
        PlacementEffect.run(path, bounds)(fa.map(_.value))
      end apply
    end new
  end run

  def withBoundsK(f : Bounds => Bounds) : Place[*] ~> Place[*] =
    new ~>[Place[*], Place[*]]:
      override def apply[A](fa : Place[A]) : Place[A] =
        PlacementEffect.withBoundsK(f)(fa)
      end apply
    end new
  end withBoundsK

  def sizeText(
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place[*]] =
    sizeTextFFI[PlacementEffect](
      PlacementEffect.getBounds.map(_.width.value),
      shaper,
      MapKCache(cache, PlacementEffect.liftK),
    )
  end sizeText

  def typecheck[TypeToCheck : Typeable](error : (Any, Path) => Throwable) : [Res] => (Any, Path, TypeToCheck => Place[Res]) => Place[Res] =
    GenericPlace.typecheck[PlacementEffect, Situated, Throwable, TypeToCheck](error)
  end typecheck

  given functorInstance : Functor[Place] =
    nestedFunctorsAreFunctors[PlacementEffect, Situated]
  end functorInstance

  def addNameToPath(name : String) : Place ~> Place =
    GenericPlace.addNameToPath(name)
  end addNameToPath

  def raiseError[Error, Value](error: => Error)(using ME: MonadError[IO, Error]): Place[Value] =
    GenericPlacementEffect.liftF(ME.raiseError(error))
  end raiseError
end Place

