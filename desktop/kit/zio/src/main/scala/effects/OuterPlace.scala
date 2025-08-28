package gui4s.desktop.kit.zio
package effects

import catnip.{Get, Set, MyStateT}
import catnip.MyStateT.given
import catnip.transformer.*
import cats.data.EitherT
import cats.*
import gui4s.core.kit.effects.OuterPlace as GenericOuterPlace
import zio.*
import zio.interop.catz.*

type OuterPlace[T] = MyStateT[Task, Bounds, T]

@SuppressWarnings(Array("org.wartremover.warts.All"))
object OuterPlace:
  given MonadError[OuterPlace, Throwable] = summon
    
  def liftK : Task ~> OuterPlace =
    MyStateT.liftK
  end liftK

  def liftF[Value](value : Task[Value]) : OuterPlace[Value] =
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

  def raiseError[Value](error : => Throwable) : OuterPlace[Value] =
    MyStateT.liftF(ZIO.fail(error))
  end raiseError

  def raiseError[Value](error : String) : OuterPlace[Value] =
    raiseError(new Exception(error))
  end raiseError

  def run(bounds : Task[Bounds]) : OuterPlace ~> Task =
    new (OuterPlace ~> Task):
      override def apply[A](place : OuterPlace[A]) : Task[A] =
        bounds.flatMap(place.eval)
      end apply
    end new
  end run
end OuterPlace
