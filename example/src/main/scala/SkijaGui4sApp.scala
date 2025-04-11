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
import me.katze.gui4s.widget.stateful.TaskFinished
import me.katze.gui4s.widget.{EventResult, given}

import scala.concurrent.ExecutionContext

def skijaApp(
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
      updateLoop(
        [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO] // TODO Сделать настоящий обработчик
      ),
      updateLoopExecutionContext
    ),
    backend => ???
  )
end skijaApp


