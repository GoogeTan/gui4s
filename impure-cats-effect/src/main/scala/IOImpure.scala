package me.katze.gui4s.impure.cats.effect

import cats.effect.IO
import me.katze.gui4s.impure.{Impure, ImpureError}

given IOImpure : Impure[IO] with ImpureError[IO, Throwable]:
  override def impure[A](trunk: => A): IO[A] =
    IO(trunk)
  end impure

  override def impureTry[T](from: => Either[Throwable, T]): IO[T] =
    IO(from).flatMap:
      case Right(value) => IO.pure(value)
      case Left(error) => IO.raiseError(error)
  end impureTry
end IOImpure