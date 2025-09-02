package gui4s.desktop.kit.zio
package effects

import catnip.{Get, Set}
import cats.*
import cats.data.EitherT
import gui4s.desktop.kit.effects.OuterPlace as GenericOuterPlace
import zio.*
import zio.interop.catz.*

type OuterPlace[T] = GenericOuterPlace[Task, T]

object OuterPlace:
  given monadInstance[IO[_] : Monad]: MonadThrow[OuterPlace] =
    GenericOuterPlace.monadInstance
  end monadInstance  

  def liftK: Task ~> OuterPlace =
    GenericOuterPlace.liftK[Task]
  end liftK

  def liftF[Value](value: Task[Value]): OuterPlace[Value] =
    liftK(value)
  end liftF

  def getBounds: Get[OuterPlace, Bounds] =
    GenericOuterPlace.getBounds[Task]
  end getBounds

  def setBounds: Set[OuterPlace, Bounds] =
    GenericOuterPlace.setBounds[Task]
  end setBounds

  def withBounds[T](original: OuterPlace[T], f: Bounds => Bounds): OuterPlace[T] =
    GenericOuterPlace.withBounds(original, f)
  end withBounds

  def raiseError[Value](error: => Throwable): OuterPlace[Value] =
    GenericOuterPlace.raiseError(error)
  end raiseError

  def run(bounds: Task[Bounds]): OuterPlace ~> EitherT[Task, Throwable, *] =
    GenericOuterPlace.run[Task](bounds)
  end run
end OuterPlace
