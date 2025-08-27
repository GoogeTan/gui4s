package gui4s.desktop.kit.zio
package effects

import catnip.{Get, Set, MyStateT}
import cats.data.EitherT
import cats.*
import gui4s.core.kit.effects.OuterPlace as GenericOuterPlace
import zio.*
import zio.interop.catz.*

type OuterPlace[T] = MyStateT[IO[String, *], Bounds, T]

object OuterPlace:
  //given MonadError[OuterPlace, String] = summon
  
  def liftK : IO[String, *] ~> OuterPlace =
    MyStateT.liftK
  end liftK

  def liftF[Value](value : UIO[Value]) : OuterPlace[Value] =
    liftK(value)
  end liftF

  def getBounds: Get[OuterPlace, Bounds] =
    MyStateT.get
  end getBounds

  def setBounds: Set[OuterPlace, Bounds] =
    MyStateT.set
  end setBounds

  def withBounds[T](original : OuterPlace[T], f : Bounds => Bounds) : OuterPlace[T] =
    for
      bounds <- getBounds
      _ <- setBounds(f(bounds))
      res <- original
      _ <- setBounds(bounds)
    yield res
  end withBounds

  def raiseError[Value](error : => String) : OuterPlace[Value] =
    MyStateT.liftF(ZIO.fail(error))
  end raiseError

  def run(bounds : IO[String, Bounds]) : OuterPlace ~> IO[String, *] =
    new (OuterPlace ~> IO[String, *]):
      override def apply[A](place : OuterPlace[A]) : IO[String, A] =
        bounds.flatMap(place.eval)
      end apply
    end new
  end run
end OuterPlace
