package me.katze.gui4s.impure.cats.effect

import cats.effect.IO
import me.katze.gui4s.impure.FFI

given IOFFI : FFI[IO]:
  override def delay[A](trunk: => A): IO[A] =
    IO.delay(trunk)
  end delay

  override def blocking[A](trunk: => A): IO[A] =
    IO.delay(trunk) // TODO это костыль, позволяющий вызывать блокинг для GLFW контекста. Надо исправить в будущем. 
  end blocking

  override def interruptible[A](trunk: => A): IO[A] =
    IO.interruptible(trunk)
  end interruptible

  override def interruptibleMany[A](trunk: => A): IO[A] =
    IO.interruptibleMany(trunk)
  end interruptibleMany
end IOFFI