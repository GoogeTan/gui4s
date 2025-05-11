package me.katze.gui4s.example
package draw

import cats.Functor
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.all.*

// TODO Почему-то ругается на эни в интерполяции строки...
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
  c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
end drawLoopExceptionHandler
