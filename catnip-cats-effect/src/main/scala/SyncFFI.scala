package catnip.cats.effect

import catnip.FFI
import cats.effect.{Sync}

final class SyncFFI[F[_] : Sync as S] extends FFI[F]:
  override def delay[A](trunk: => A): F[A] =
    S.delay(trunk)
  end delay

  override def blocking[A](trunk: => A): F[A] =
    S.delay(trunk) // TODO это костыль, позволяющий вызывать блокинг для GLFW контекста. Надо исправить в будущем.
  end blocking

  override def interruptible[A](trunk: => A): F[A] =
    S.interruptible(trunk)
  end interruptible

  override def interruptibleMany[A](trunk: => A): F[A] =
    S.interruptibleMany(trunk)
  end interruptibleMany
end SyncFFI