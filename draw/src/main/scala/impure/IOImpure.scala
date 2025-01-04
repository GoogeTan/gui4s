package me.katze.gui4s.draw
package impure

import cats.effect.IO

object IOImpure extends Impure[IO] with ImpureError[IO, Throwable]:
  override def impure[A](trunk: => A): IO[A] =
    IO(trunk)
  end impure

  override def impureTry[T](from: => Either[Throwable, T]): IO[T] =
    IO(from).flatMap:
      case Right(value) => IO.pure(value)
      case Left(error) => IO.raiseError(error)
  end impureTry
end IOImpure
