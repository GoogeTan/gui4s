package gui4s.desktop.kit
package common.effects

import common.effects.InnerPlace.given
import common.effects.OuterPlace.given
import common.*

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import catnip.{ForeignFunctionInterface, MapKCache}
import cats.data.EitherT
import cats.{Functor, Monad, MonadThrow, ~>}
import gui4s.core.kit.effects.Place as GenericPlace
import gui4s.core.widget.Path
import io.github.humbleui.skija.shaper.Shaper

import scala.reflect.Typeable

type Place[IO[_], T] = GenericPlace[IO, Bounds, Float, Throwable, T]
type PlaceC[IO[_]] = [Value] =>> Place[IO, Value]

object Place:
  def run[IO[_] : Monad](bounds : IO[Bounds]) : Place[IO, *] ~> EitherT[IO, Throwable, *] =
    new ~>[Place[IO, *], EitherT[IO, Throwable, *]]:
      override def apply[A](fa : Place[IO, A]) : EitherT[IO, Throwable, A] =
        OuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText[IO[_] : {Monad, ForeignFunctionInterface as ffi}](
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place[IO, *]] =
    sizeTextFFI[OuterPlace[IO, *]](
      OuterPlace.getBounds.map(_.width.value),
      ffi.mapK(OuterPlace.liftK),
      shaper,
      MapKCache(cache, OuterPlace.liftK),
    )
  end sizeText

  def typecheck[IO[_] : MonadThrow, U : Typeable](error : (Any, Path) => Throwable) : [T] => (Any, Path, U => Place[IO, T]) => Place[IO, T] =
    GenericPlace.typecheck[OuterPlace[IO, *], InnerPlace, Throwable, U](error)
  end typecheck

  given functorInstance[IO[_] : Monad] : Functor[Place[IO, *]] = nestedFunctorsAreFunctors[OuterPlace[IO, *], InnerPlace]
end Place
