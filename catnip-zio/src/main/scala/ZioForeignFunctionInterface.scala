package catnip.zio

import catnip.ForeignFunctionInterface
import zio.*

final class ZIOForeignFunctionInterface extends ForeignFunctionInterface[Task]:
  override def delay[A](trunk: => A): Task[A] =
    ZIO.attempt(trunk)

  override def blocking[A](trunk: => A): Task[A] =
    ZIO.attemptBlocking(trunk)

  override def interruptible[A](trunk: => A): Task[A] =
    ZIO.attemptBlockingInterrupt(trunk)

  override def interruptibleMany[A](trunk: => A): Task[A] =
    ZIO.attemptBlockingInterrupt(trunk)
end ZIOForeignFunctionInterface

final class ZioForeignFunctionInterface[Ctx, Error] extends ForeignFunctionInterface[ZIO[Ctx, Error, *]]]:
  override def delay[A](trunk: => A): ZIO[Ctx, Error, A] =
    S.delay(trunk)
  end delay

  override def blocking[A](trunk: => A):  ZIO[Ctx, Error, A] =
    S.blocking(trunk)
  end blocking

  override def interruptible[A](trunk: => A): ZIO[Ctx, Error, A]=
    S.interruptible(trunk)
  end interruptible

  override def interruptibleMany[A](trunk: => A): ZIO[Ctx, Error, A] =
    S.interruptibleMany(trunk)
  end interruptibleMany
end ZioForeignFunctionInterface

