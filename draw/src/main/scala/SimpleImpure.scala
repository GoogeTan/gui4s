package me.katze.gui4s.draw

import cats.effect.IO

object SimpleImpure extends Impure[IO]:
  override def impure[A](trunk: => A): IO[A] =
    IO(trunk)
  end impure
end SimpleImpure
