package gui4s.desktop.kit.cats
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

type Place[T] = GenericPlace[IO, Bounds, Float, String, T]

object Place:
  def run(bounds : IO[Bounds]) : Place ~> EitherT[IO, String, *] =
    GenericPlace.run(bounds)
  end run

  def sizeText(
    ffi : ForeignFunctionInterface[IO],
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place] =
    import OuterPlace.given
    sizeTextFFI[OuterPlace](
      OuterPlace.getBounds.map(_.width.value),
      ffi.mapK(OuterPlace.liftK),
      shaper,
      MapKCache(cache, OuterPlace.liftK),
    )
  end sizeText

  def typecheck[U : Typeable](error : (Any, Path) => String) : [T] => (Any, Path, U => Place[T]) => Place[T] =
    GenericPlace.typecheck[IO, Bounds, Float, String, U](error)
  end typecheck

  given Functor[Place] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]
end Place