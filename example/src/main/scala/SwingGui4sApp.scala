package me.katze.gui4s.example

import draw.swing.{*, given}
import draw.{*, given}
import impl.{WindowResized, given}
import place.RunPlacement
import task.{EventProducingEffectT, RunnableIO}
import update.ApplicationRequest

import cats.data.ReaderT
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import me.katze.gui4s.example.given
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}
import me.katze.gui4s.widget.{EventResult, Widget, given}

import scala.concurrent.ExecutionContext

type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
type Task[T] = RunnableIO[EventProducingEffectT[IO], T]
type Recomposition = IO[Unit]

def swingApp(
              rootWidget : MeasurableT[IO, Float][
                Widget[Update[Task[Any]], SwingDraw[IO, Float, Unit], MeasurableT[IO, Float], Recomposition, ApplicationRequest, TaskFinished | WindowResized.type]
              ],
              updateLoopExecutionContext : ExecutionContext,
              drawLoopExecutionContext: ExecutionContext,
            ) : IO[ExitCode] =
  runApplicationLoops[
    IO,
    TaskFinished | WindowResized.type,
    [B] =>> RootWidget[
      IO,
      SwingDraw[IO, Float, Unit],
      MeasurableT[IO, Float],
      Update[Task[Any]],
      Recomposition,
      ApplicationRequest,
      B,
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
            Update[Task[Any]],
            RootWidget[IO, SwingDraw[IO, Float, Unit], MeasurableT[IO, Float], Update[Task[Any]], Recomposition, ApplicationRequest, TaskFinished | WindowResized.type],
            ApplicationRequest,
            TaskFinished | WindowResized.type
          ](
            [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO] // TODO Сделать настоящий обработчик
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
