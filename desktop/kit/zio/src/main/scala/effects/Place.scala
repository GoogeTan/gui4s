package gui4s.desktop.kit.zio
package effects

import effects.InnerPlace.given
import effects.OuterPlace.given

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import catnip.{ForeignFunctionInterface, MapKCache}
import cats.data.EitherT
import cats.effect.IO
import cats.{Functor, Monad, MonadThrow, ~>}
import gui4s.desktop.kit.common.effects.Place.given
import gui4s.core.widget.Path
import gui4s.desktop.kit.common.effects.Place as GenericPlace
import io.github.humbleui.skija.shaper.Shaper

import scala.reflect.Typeable
import zio.*
import zio.interop.catz.*

type Place[T] = GenericPlace[Task, T]

object Place:
  def run(bounds : Task[Bounds]) : Place ~> EitherT[Task, Throwable, *] =
    GenericPlace.run(bounds)
  end run
  
  def sizeText(
    shaper : Shaper,
    cache : TextCache[Task],
  ) : SizeText[Place] =
    GenericPlace.sizeText(shaper, cache)
  end sizeText

  def typecheck[U : Typeable](error : (Any, Path) => Throwable) : [T] => (Any, Path, U => Place[T]) => Place[T] =
    GenericPlace.typecheck[Task, U](error)
  end typecheck

  given functorInstance: Functor[Place] = GenericPlace.functorInstance
end Place
