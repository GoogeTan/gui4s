package me.katze.gui4s.example

import api.impl.{HighLevelApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{LayoutApi, TextWidgetApi}
import draw.*
import impl.{*, given}
import place.*
import update.*

import cats.*
import cats.effect.*
import cats.effect.std.{Queue, QueueSink}
import cats.syntax.all.*
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{LayoutDraw, LiftEventReaction, TextDraw, TextPlacement}
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, Path, RaiseEvent}

import scala.concurrent.ExecutionContext

def runApplicationLoops[
  F[+_] : Async,
  Place[+_],
  Update[+_, +_],
  MeasurementUnit,
  Draw,
  UpEvent,
  DownEvent,
  RootWidget[+A, -B] <: EventConsumer[Update, F[RootWidget[A, B]], A, B] & Drawable[Draw],
](
    drawApi: QueueSink[F, DownEvent] => Resource[F, (
        RunPlacement[F, Place], 
        DrawLoop[F, Drawable[Draw]], 
        Place[RootWidget[UpEvent, DownEvent]]
      )
    ],
    updateLoopExecutionContext : ExecutionContext,
    runUpdate: [A] => Update[A, UpEvent] => F[Either[ExitCode, A]]
) : F[ExitCode] =
  for
    eventBus <- Queue.unbounded[F, DownEvent]
    code <- drawApi(eventBus).use((runPlacement, drawLoop, freeRootWidget) =>
      given runPlacement.type = runPlacement
      for
        rootWidget <- freeRootWidget.runPlacement
        widgetCell <- Ref[F].of(rootWidget)
        code <- applicationLoop(
          eventBus,
          widgetCell,
          drawLoop,
          runUpdateLoopOn(
            updateLoop(runUpdate),
            updateLoopExecutionContext
          )
        ).flatMap(_.join)
      yield code
    )
  yield code
end runApplicationLoops
