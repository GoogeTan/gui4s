package gui4s.desktop.kit.cats
package effects

import catnip.{Get, Set}
import cats.data.EitherT
import cats.effect.IO
import cats.{Monad, ~>}
import gui4s.core.kit.effects.OuterPlace as GenericOuterPlace

type OuterPlace[T] = GenericOuterPlace[IO, Bounds, String, T]

object OuterPlace:
  given Monad[OuterPlace] = GenericOuterPlace.monadInstance

  def liftK : IO ~> OuterPlace =
    GenericOuterPlace.liftK
  end liftK

  def liftF[Value](value : IO[Value]) : OuterPlace[Value] =
    GenericOuterPlace.liftF(value)
  end liftF

  def getBounds: Get[OuterPlace, Bounds] =
    GenericOuterPlace.getBounds
  end getBounds

  def setBounds: Set[OuterPlace, Bounds] =
    GenericOuterPlace.setBounds
  end setBounds

  def withBounds[T](original : OuterPlace[T], f : Bounds => Bounds) : OuterPlace[T] =
    GenericOuterPlace.withBounds(original, f)
  end withBounds

  def raiseError[Value](error : => String) : OuterPlace[Value] =
    GenericOuterPlace.raiseError(error)
  end raiseError

  def run(bounds : IO[Bounds]) : OuterPlace ~> EitherT[IO, String, *] =
    GenericOuterPlace.run(bounds)
  end run
end OuterPlace