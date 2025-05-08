package me.katze.gui4s.example

import draw.swing.{*, given}
import draw.{*, given}
import impl.WindowResized
import place.RunPlacement
import task.{EventProducingEffectT, RunnableIO}
import update.ApplicationRequest

import cats.data.ReaderT
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished, given}
import me.katze.gui4s.widget.{EventResult, Widget, given}

import scala.concurrent.ExecutionContext

type Update[UpEvent] = [Value] =>> EventResult[Value, UpEvent]
type Recomposition[F[_]] = F[Unit]

def swingApp(
              rootWidget : MeasurableT[IO, Float][
                Widget[Update[ApplicationRequest], SwingDraw[IO, Float, Unit], MeasurableT[IO, Float], Recomposition[IO], TaskFinished | WindowResized.type]
              ],
              updateLoopExecutionContext : ExecutionContext,
              drawLoopExecutionContext: ExecutionContext,
            ) : IO[ExitCode] =
  runApplicationLoops[
    IO,
    TaskFinished | WindowResized.type,
    [DownEvent] =>> RootWidget[
      IO,
      SwingDraw[IO, Float, Unit],
      MeasurableT[IO, Float],
      Update[ApplicationRequest],
      Recomposition[IO],
      DownEvent,
    ]
  ](
    downEventSink => SwingApi[IO, Float, SwingDraw[IO, Float, Unit]](
      IOImpure,
      downEventSink.offer(WindowResized),
      ReaderT[IO, SwingDrawState[Float], Unit]
    ).map(api =>
      given rp: RunPlacement[IO, MeasurableT[IO, Float]] = MeasurableRunPlacement(api.windowBounds)
      //swingLayoutDraw[SwingDraw[IO, Float, Unit], Float],
      //swingTextDraw(using api.graphics)
      (
        runDrawLoopOn(
          simpleGraphicsDrawLoop[IO, Float, SwingDraw[IO, Float, Unit]](api.graphics, runSwingDraw),
          drawLoopExecutionContext
        ),
        runUpdateLoopOn(
          updateLoop[
            IO,
            Update[ApplicationRequest],
            RootWidget[IO, SwingDraw[IO, Float, Unit], MeasurableT[IO, Float], Update[ApplicationRequest], Recomposition[IO], TaskFinished | WindowResized.type],
            TaskFinished | WindowResized.type
          ](
            [Event] => (update : Update[ApplicationRequest][Event]) => Right(update.widget).pure[IO] // TODO Сделать настоящий обработчик
          ),
          updateLoopExecutionContext
        ),
        rootWidget.map(wt =>
          RootWidget(
            Path(List("ROOT")),
            wt,
            identity
          )
        ).runPlacement,
      ),
    ),
  )
end swingApp
