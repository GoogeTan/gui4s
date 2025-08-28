package gui4s.desktop.kit.zio
package effects

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.data.EitherT
import cats.{Functor, ~>}
import io.github.humbleui.skija.shaper.Shaper
import gui4s.core.kit.effects.Place as GenericPlace
import gui4s.core.widget.Path
import effects.OuterPlace.given
import effects.InnerPlace.given
import gui4s.desktop.kit.zio.{SizeText, TextCache, sizeTextFFI}
import catnip.transformer.given_Monad_MyStateT
import catnip.{ForeignFunctionInterface, MapKCache}
import catnip.effect.*

import zio.*
import zio.interop.catz.*
import scala.reflect.Typeable

type Place[T] = OuterPlace[InnerPlace[T]]

object Place:
  def run(bounds : Task[Bounds]) : Place ~> Task =
    new ~>[Place, Task]:
      override def apply[A](fa : Place[A]) : Task[A] =
        OuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText(
    shaper : Shaper,
    cache : TextCache[Task],
  ) : SizeText[Place] =
    sizeTextFFI[OuterPlace](
      OuterPlace.getBounds.map(_.width.value),
      SyncForeignFunctionInterface[Task].mapK(OuterPlace.liftK),
      shaper,
      MapKCache(cache, OuterPlace.liftK),
    )
  end sizeText

  def typecheck[U : Typeable](error : (Any, Path) => Throwable) : [T] => (Any, Path, U => Place[T]) => Place[T] =
    GenericPlace.typecheck[OuterPlace, InnerPlace, Throwable, U](error)
  end typecheck

  def typecheckS[U : Typeable](error : (Any, Path) => String) : [T] => (Any, Path, U => Place[T]) => Place[T] =
    typecheck[U]((value : Any, path : Path) => new Exception(error(value, path)))
  end typecheckS

  given Functor[Place] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]
end Place
