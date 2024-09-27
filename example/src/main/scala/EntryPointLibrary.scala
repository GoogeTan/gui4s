package me.katze.gui4s.example

import draw.*
import update.*

import cats.Monad
import cats.effect.kernel.Concurrent
import cats.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import me.katze.gui4s.widget.stateful.*


trait EntryPointLibrary[F[+_] : Concurrent : ProcessRequest, Bounds, A, B] extends RootLibrary[F, F[Unit], Bounds]/*:
  
  def run(args: List[String]): F[ExitCode] =
    for
      api <- initDrawApi
      widget <- widget(args)
      a <- applicationLoop(
        widget,
        drawLoop(drawLoopExceptionHandler, api),
        updateLoop
      )
      code <- a.join
    yield code
  end run

  def initDrawApi : F[SimpleDrawApi[F]]

  def widget(args : List[String]) : F[FreeRootWidget[A, B]]
  
  def drawLoopExceptionHandler(exception: Throwable): F[Option[ExitCode]] /*=
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler*/
end EntryPointLibrary*/
