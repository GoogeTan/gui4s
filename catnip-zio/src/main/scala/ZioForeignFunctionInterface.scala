package catnip.zio

import catnip.ForeignFunctionInterface
import zio.*

final class ZioForeignFunctionInterface extends ForeignFunctionInterface[Task]:
  override def delay[A](trunk: => A): Task[A] =
    ZIO.attempt(trunk)

  override def blocking[A](trunk: => A): Task[A] =
    ZIO.attemptBlocking(trunk)

  override def interruptible[A](trunk: => A): Task[A] =
    ZIO.attemptBlockingInterrupt(trunk)

  override def interruptibleMany[A](trunk: => A): Task[A] =
    ZIO.attemptBlockingInterrupt(trunk)
end ZioForeignFunctionInterface

