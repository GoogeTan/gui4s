package gui4s.desktop.kit.cats
package effects

import cats.data.EitherT
import cats.effect.IO
import cats.{Functor, ~>}
import gui4s.core.widget.Path
import gui4s.desktop.kit.common.*
import gui4s.desktop.kit.common.effects.Place as GenericPlace
import io.github.humbleui.skija.shaper.Shaper

import scala.reflect.Typeable

type Place[T] = GenericPlace[IO, T]

object Place:
  def run(bounds : IO[Bounds]) : Place ~> EitherT[IO, Throwable, *] =
    GenericPlace.run(bounds)
  end run
  
  def sizeText(
    shaper : Shaper,
    cache : TextCache[IO],
  ) : SizeText[Place] =
    GenericPlace.sizeText(shaper, cache)
  end sizeText

  def typecheck[U : Typeable](error : (Any, Path) => Throwable) : [T] => (Any, Path, U => Place[T]) => Place[T] =
    GenericPlace.typecheck[IO, U](error)
  end typecheck

  given functorInstance: Functor[Place] = GenericPlace.functorInstance
end Place
