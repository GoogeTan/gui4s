package me.katze.gui4s.example

import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import cats.Monad
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.stateful.{Path, TaskFinished, given}
import me.katze.gui4s.widget.{Widget, given}

import scala.concurrent.ExecutionContext

def skijaApp[F[+_] : {Async, Console, Impure}](
                                                widget : SkijaBackend[F, OglWindow] ?=> Measurable[F, Float,
                                                  Widget[
                                                    Update[ApplicationRequest],
                                                    SkijaDraw[F, OglWindow],
                                                    MeasurableT[F, Float],
                                                    Recomposition[F],
                                                    TaskFinished
                                                  ]
                                                ],
                                                backendCreationExecutionContext : ExecutionContext,
                                                updateLoopExecutionContext : ExecutionContext,
                                                drawLoopExecutionContext: ExecutionContext,
                                              ) =
  type SkijaRootWidget[DownEvent] = RootWidget[
    F,
    SkijaDraw[F, OglWindow],
    MeasurableT[F, Float],
    Update[ApplicationRequest],
    Recomposition[F],
    DownEvent,
  ]

  runApplicationLoopsWithBackend[
    F,
    TaskFinished,
    SkijaRootWidget,
    SkijaBackend[F, OglWindow]
  ](
    backend = downEventSink => SkijaSimpleDrawApi.createForTests[F].evalOn(backendCreationExecutionContext),
    drawLoop = backend => runDrawLoopOnExecutionContext(
      skijaDrawLoop[F, OglWindow](backend),
      drawLoopExecutionContext
    ),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, SkijaRootWidget, TaskFinished](
      updateLoop(handleApplicationRequests),
      updateLoopExecutionContext
    ),
    rootWidget = backend =>
      given RunPlacement[F, MeasurableT[F, Float]] = MeasurableRunPlacement[F, F, Float](backend.windowBounds)

      measurableIsFlatMap[F, Float].map(widget(using backend))(widget =>
        RootWidget(
          Path(List("ROOT")),
          widget,
          identity
        )
      ).runPlacement
  )
end skijaApp

def handleApplicationRequests[F[_] : Monad] : [T] => Update[ApplicationRequest][T] => F[Either[ExitCode, T]] =
  [T] => update => update.events.foldM(Right(update.widget))((_, request) =>
    request match
      case ApplicationRequest.CloseApp(code) => Left(code).pure[F]
  )
end handleApplicationRequests
