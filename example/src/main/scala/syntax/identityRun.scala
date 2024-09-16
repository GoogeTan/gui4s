package me.katze.gui4s.example
package syntax

import application.Run

given identityRun[F[_]] : Run[F, F] with
  override def run[T](value: F[T]): F[T] =
    value
  end run
end identityRun
