package me.katze.gui4s.example

import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import cats.effect.Async
import cats.effect.std.Console
import cats.syntax.all.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.impure.cats.effect.given
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished, given}
import me.katze.gui4s.widget.{Widget, given}

import scala.concurrent.ExecutionContext

def skijaApp[F[+_] : {Async, Console, Impure}](
                                                widget : SkijaBackend[F, OglWindow] => Measurable[F, Float,
                                                  Widget[
                                                    Update[ApplicationRequest],
                                                    SkijaDraw[F, OglWindow],
                                                    MeasurableT[F, Float],
                                                    Recomposition[F],
                                                    TaskFinished
                                                  ]
                                                ],
                                                updateLoopExecutionContext : ExecutionContext,
                                                drawLoopExecutionContext: ExecutionContext,
                                              ) =
  runApplicationLoopsWithBackend[
    F,
    TaskFinished,
    [B] =>> RootWidget[
      F,
      SkijaDraw[F, OglWindow],
      MeasurableT[F, Float],
      Update[ApplicationRequest],
      Recomposition[F],
      B,
    ],
    SkijaBackend[F, OglWindow]
  ](
    downEventSink => SkijaSimpleDrawApi.createForTests[F].evalOn(drawLoopExecutionContext),
    backend => runDrawLoopOn(
      skijaDrawLoop[F, OglWindow](backend),
      drawLoopExecutionContext
    ),
    backend => runUpdateLoopOn[F, [B] =>> RootWidget[
      F,
      SkijaDraw[F, OglWindow],
      MeasurableT[F, Float],
      Update[ApplicationRequest],
      Recomposition[F],
      B,
    ], TaskFinished](
      updateLoop[
        F,
        Update[ApplicationRequest],
        RootWidget[F, SkijaDraw[F, OglWindow], MeasurableT[F, Float], Update[ApplicationRequest], F[Unit], TaskFinished],
        TaskFinished
      ](
        [Event] => (update : Update[ApplicationRequest][Event]) => Right(update.widget).pure[F] // TODO Сделать настоящий обработчик
      ),
      updateLoopExecutionContext
    ),
    backend =>
      given RunPlacement[F, MeasurableT[F, Float]] = MeasurableRunPlacement[F, F, Float](backend.windowBounds)

      measurableIsFlatMap[F, Float].map(widget(backend))(widget =>
        RootWidget[
          F,
          SkijaDraw[F, OglWindow],
          MeasurableT[F, Float],
          Update[ApplicationRequest],
          Recomposition[F],
          TaskFinished,
        ](
          Path(List("ROOT")),
          widget,
          identity
        )
      ).runPlacement
  )
end skijaApp


