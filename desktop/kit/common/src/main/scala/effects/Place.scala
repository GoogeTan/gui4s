package gui4s.desktop.kit
package effects

import catnip.{ForeignFunctionInterface, MapKCache}
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.data.EitherT
import cats.effect.IO
import cats.{Functor, ~>}
import io.github.humbleui.skija.shaper.Shaper
import gui4s.core.kit.effects.Place as GenericPlace
import gui4s.core.widget.Path
import OuterPlace.given
import InnerPlace.given

import scala.reflect.Typeable

type Place[IO[_], T] = GenericPlace[IO, Bounds, Float, String, T]
type PlaceC[IO[_]] = [Value] =>> Place[IO, Value]

trait PlaceOps:
  def run[IO[_]](bounds : IO[Bounds]) : Place[IO, *] ~> EitherT[IO, String, *] =
    new ~>[Place, EitherT[IO, String, *]]:
      override def apply[A](fa : Place[A]) : EitherT[IO, String, A] =
        OuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText[IO[_]](
    ffi : ForeignFunctionInterface[IO],
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place[IO, *]] =
    import OuterPlace.given
    sizeTextFFI[OuterPlace[IO, *]](
      OuterPlace.getBounds.map(_.width.value),
      ffi.mapK(OuterPlace.liftK),
      shaper,
      MapKCache(cache, OuterPlace.liftK),
    )
  end sizeText

  def typecheck[IO[_], U : Typeable](error : (Any, Path) => String) : [T] => (Any, Path, U => Place[IO, T]) => Place[IO, T] =
    GenericPlace.typecheck[OuterPlace[IO, *], InnerPlace, String, U](error)
  end typecheck

  given[IO[_] : Functor] : Functor[Place[IO, *]] = nestedFunctorsAreFunctors[OuterPlace[IO, *], InnerPlace]
end Place
