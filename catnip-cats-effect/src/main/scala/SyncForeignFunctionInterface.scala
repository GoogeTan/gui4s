package catnip.effect

import catnip.ForeignFunctionInterface
import cats.effect.Sync

final class SyncForeignFunctionInterface[F[_] : Sync as S] extends ForeignFunctionInterface[F]:
  override def delay[A](trunk: => A): F[A] =
    S.delay(trunk)
  end delay

  override def blocking[A](trunk: => A): F[A] =
    S.blocking(trunk)
  end blocking

  override def interruptible[A](trunk: => A): F[A] =
    S.interruptible(trunk)
  end interruptible

  override def interruptibleMany[A](trunk: => A): F[A] =
    S.interruptibleMany(trunk)
  end interruptibleMany
end SyncForeignFunctionInterface