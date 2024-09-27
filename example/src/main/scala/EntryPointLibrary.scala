package me.katze.gui4s.example

import draw.*
import place.ApplicationBounds
import root.{RootPlacedWidget, RootWidgetFree}
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.impl.WidgetLibraryImpl
import me.katze.gui4s.widget.library.{LabelDraw, LabelLibrary, LabelPlacement, WidgetLibrary}
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

class Library[F[+_] : Monad, MU](api : SimpleDrawApi[F]) extends WidgetLibraryImpl[F, F[Unit], Bounds[MU]] with LabelLibrary[Unit]:
  override def textIsPlaceable: LabelPlacement[Placeable[Bounds[MU], Unit]] = _ => _ => ()

  override def textDraw: LabelDraw[F[Unit], Unit] = (text, _) => api.text(50, 50, text, TextStyle(18, 0, 400))
end Library


object ExampleApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- initSwing
      lib = Library[IO, Int](swing.graphics)
      code <- runWidget(lib)(
        lib.label("123213534634346435456343456"),
        drawLoopExceptionHandler,
        swing.graphics,
      )(using summon, SwingProcessRequest(swing), SwingApplicationBounds(swing))
    yield code
  end run

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end ExampleApp


