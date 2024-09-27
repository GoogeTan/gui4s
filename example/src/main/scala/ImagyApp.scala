package me.katze.gui4s.example

import draw.*
import update.*

import cats.effect.{ExitCode, IO, IOApp}
import me.katze.gui4s.widget.stateful.TaskFinished

trait ImagyApp[
  PlacedWidget[A, B] <: Drawable[IO[Unit]] & EventConsumer[FreeWidget[A, ApplicationRequest], IO, A, B],
  FreeWidget[A, B] <: Placeable[IO, PlacedWidget[A, B]],
  DownEvent
](
  using  ProcessRequest[IO]
) extends IOApp:
  final type DefaultWidgetP[A] = PlacedWidget[A | TaskFinished, ApplicationRequest]
  final type DefaultWidgetF[A] = FreeWidget[A | TaskFinished, ApplicationRequest]
  
  override def run(args: List[String]): IO[ExitCode] =
    for
      api <- initDrawApi
      widget <- widget(args)
      a <- applicationLoop[IO, DownEvent | ApplicationEvent, DefaultWidgetP, DefaultWidgetF](
        widget,
        drawLoop(drawLoopExceptionHandler, api),
        updateLoop[IO, DefaultWidgetP, DefaultWidgetF, DownEvent | ApplicationEvent]
      )
      code <- a.join
    yield code
  end run

  def initDrawApi : IO[SimpleDrawApi[IO]]

  def widget(args : List[String]) : IO[DefaultWidgetF[DownEvent | ApplicationEvent]]
  
  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end ImagyApp
