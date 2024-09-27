package me.katze.gui4s.example

import draw.{SimpleDrawApi, SwingApplicationBounds, SwingProcessRequest, initSwing}
import update.ApplicationRequest

import cats.effect.{ExitCode, IO, IOApp}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.impl.WidgetLibraryImpl


trait SwingApp extends IOApp:
  type Library <: WidgetLibraryImpl[IO, IO[Unit], Bounds[Int]]

  final override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- initSwing
      lib = createLibrary(swing.graphics)
      code <- runWidget(lib)(
        app(lib),
        drawLoopExceptionHandler,
        swing.graphics,
      )(using summon, SwingProcessRequest(swing), SwingApplicationBounds(swing))
    yield code
  end run

  def createLibrary(drawApi : SimpleDrawApi[IO]) : Library

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler

  def app(api : Library) : api.Widget[ApplicationRequest]
end SwingApp
