package me.katze.gui4s.example

import draw.*
import draw.skija.{SkijaSimpleDrawApi, *}
import impl.{ENErrors, WindowResized, containerPlacementCurried}
import place.RunPlacement
import task.{EventProducingEffectT, RunnableIO}
import update.ApplicationRequest

import cats.effect.IO
import cats.syntax.all.*
import cats.{Applicative, Monad}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.cats.effect.{IOImpure, given}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}
import me.katze.gui4s.widget.{EventResult, Widget, given}

import scala.concurrent.ExecutionContext

def skijaApp(
              widget : SkijaBackend[IO] => Measurable[IO, Float,
                Widget[
                  Update[Task[Any]],
                  SkijaDraw[IO, OglWindow],
                  MeasurableT[IO, Float],
                  Recomposition,
                  ApplicationRequest,
                  TaskFinished
                ]
              ],
              updateLoopExecutionContext : ExecutionContext,
              drawLoopExecutionContext: ExecutionContext,
            ) =
  runApplicationLoopsWithBackend[
    IO,
    TaskFinished,
    [B] =>> RootWidget[
      IO,
      SkijaDraw[IO, OglWindow],
      MeasurableT[IO, Float],
      Update[Task[Any]],
      Recomposition,
      ApplicationRequest,
      B,
    ],
    SkijaBackend[IO]
  ](
    downEventSink => SkijaSimpleDrawApi.createForTests,
    backend => runDrawLoopOn(
      skijaDrawLoop[IO, OglWindow],
      drawLoopExecutionContext
    ),
    backend => runUpdateLoopOn(
      updateLoop[
        IO,
        Update[Task[Any]], 
        RootWidget[IO, SkijaDraw[IO, OglWindow], MeasurableT[IO, Float], Update[Task[Any]], IO[Unit], ApplicationRequest, TaskFinished],
        ApplicationRequest,
        TaskFinished
      ](
        [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO] // TODO Сделать настоящий обработчик
      ),
      updateLoopExecutionContext
    ),
    backend =>
      given RunPlacement[IO, MeasurableT[IO, Float]] = MeasurableRunPlacement(backend.windowBounds)
      widget(backend).map(widget =>
        RootWidget[
          IO,
          SkijaDraw[IO, OglWindow],
          MeasurableT[IO, Float],
          Update[Task[Any]],
          Recomposition,
          ApplicationRequest,
          TaskFinished,
        ](
          Path(List("ROOT")),
          widget,
          identity
        )
      ).runPlacement
  )
end skijaApp


