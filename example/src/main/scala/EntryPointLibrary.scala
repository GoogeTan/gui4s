package me.katze.gui4s.example

import draw.*
import update.ApplicationRequest

import cats.*
import cats.effect.*
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.impl.{StatefulWidgetLibraryImpl, WidgetLibraryImpl}
import me.katze.gui4s.widget.library.{LabelDraw, LabelLibrary, LabelPlacement, StatefulLibrary}
import me.katze.gui4s.widget.placeable.Placeable

class SimpleDrawApiLibrary[F[+_] : Monad, MU](api : SimpleDrawApi[F]) extends StatefulWidgetLibraryImpl[F, F[Unit], Bounds[MU]] with LabelLibrary with StatefulLibrary:
  override type LabelPlacementMeta = Unit

  override def textIsPlaceable: LabelPlacement[Placeable[Bounds[MU], Unit]] = _ => _ => ()

  override def textDraw: LabelDraw[F[Unit], Unit] = (text, _) => api.text(0, 10, text, TextStyle(18, 0, 400))
end SimpleDrawApiLibrary

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


object ExampleApp extends SwingApp:
  override def app(api: Library): api.Widget[ApplicationRequest] =
    api.label("12345")
  end app

  override type Library = SimpleDrawApiLibrary[IO, Int]
  
  override def createLibrary(drawApi: SimpleDrawApi[IO]) = SimpleDrawApiLibrary(drawApi)
end ExampleApp


