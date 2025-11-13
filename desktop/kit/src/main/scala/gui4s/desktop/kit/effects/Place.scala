package gui4s.desktop.kit
package effects

import catnip.MapKCache
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.data.EitherT
import cats.effect.Sync
import cats.{Functor, Monad, MonadThrow, ~>}
import gui4s.core.kit.effects.Place as GenericPlace
import gui4s.core.widget.Path
import gui4s.desktop.kit.effects.InnerPlace.given
import gui4s.desktop.kit.effects.OuterPlace.given
import io.github.humbleui.skija.shaper.Shaper

import scala.reflect.Typeable

type Place[IO[_], T] = GenericPlace[IO, Bounds, Float, Throwable, T]
type PlaceC[IO[_]] = [Value] =>> Place[IO, Value]

object Place:
  def run[IO[_] : Monad](path : Path, bounds : IO[Bounds]) : Place[IO, *] ~> EitherT[IO, Throwable, *] =
    new ~>[Place[IO, *], EitherT[IO, Throwable, *]]:
      override def apply[A](fa : Place[IO, A]) : EitherT[IO, Throwable, A] =
        OuterPlace.run(path, bounds)(fa.map(_.value))
      end apply
    end new
  end run

  def withBoundsK[IO[_] : Sync](f : Bounds => Bounds) : Place[IO, *] ~> Place[IO, *] =
    new ~>[Place[IO, *], Place[IO, *]]:
      override def apply[A](fa : Place[IO, A]) : Place[IO, A] =
        OuterPlace.withBoundsK(f)(fa)
      end apply
    end new
  end withBoundsK
  
  def sizeText[IO[_] : Sync](
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place[IO, *]] =
    sizeTextFFI[OuterPlace[IO, *]](
      OuterPlace.getBounds.map(_.width.value),
      shaper,
      MapKCache(cache, OuterPlace.liftK),
    )
  end sizeText

  def typecheck[IO[_] : MonadThrow, U : Typeable](error : (Any, Path) => Throwable) : [T] => (Any, Path, U => Place[IO, T]) => Place[IO, T] =
    GenericPlace.typecheck[OuterPlace[IO, *], InnerPlace, Throwable, U](error)
  end typecheck

  given functorInstance[IO[_] : Monad] : Functor[Place[IO, *]] =
    nestedFunctorsAreFunctors[OuterPlace[IO, *], InnerPlace]
  end functorInstance

  def addNameToPath[IO[_] : Monad](name : String) : Place[IO, *] ~> Place[IO, *] =
    GenericPlace.addNameToPath(name)
  end addNameToPath
end Place
