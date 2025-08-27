package gui4s.desktop.kit.zio
package effects

import catnip.{ForeignFunctionInterface, MapKCache}
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.data.EitherT
import cats.{Functor, ~>}
import io.github.humbleui.skija.shaper.Shaper
import gui4s.core.kit.effects.Place as GenericPlace
import gui4s.core.widget.Path
import effects.OuterPlace.given
import effects.InnerPlace.given

import zio.*
import zio.interop.catz.*


import scala.reflect.Typeable

type Place[T] = OuterPlace[InnerPlace[T]]

object Place:
  def run(bounds : IO[String, Bounds]) : Place ~> IO[String, *] =
    new ~>[Place, IO[String, *]]:
      override def apply[A](fa : Place[A]) : IO[String, A] =
        OuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText(
    ffi : ForeignFunctionInterface[UIO],
    shaper : Shaper,
    cache : TextCache[UIO],
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
