package me.katze.gui4s.example

import catnip.syntax.bimonad.{*, given}
import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import catnip.FFI
import cats.Monad
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.example.api.{PlacedWidget, Recomposition, Update, Widget, skijaWidgetHandlesEvent, skijaWidgetHasInnerStates, skijaWidgetIsDrawable, skijaWidgetReactsOnRecomposition}
import me.katze.gui4s.glfw.{KeyAction, KeyModes, OglWindow, Size}
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.{Path, given}

import scala.concurrent.ExecutionContext

enum SkijaDownEvent:
  case WindowResized
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(x: Double, y: Double)
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)

def skijaApp[F[+_] : {Async, Console, FFI}](
    widget: SkijaBackend[F, OglWindow] ?=> Widget[F, ApplicationRequest, SkijaDownEvent],
    updateLoopExecutionContext: ExecutionContext,
    drawLoopExecutionContext: ExecutionContext,
) =
  type SkijaRootWidget[DownEvent] = RootWidget[
    F,
    PlacedWidget[F, ApplicationRequest, SkijaDownEvent],
    SkijaDraw[F, OglWindow],
    MeasurableT[F, Float],
    Update[ApplicationRequest],
    Recomposition[F],
    DownEvent,
  ]

  runApplicationLoopsWithBackend[
    F,
    SkijaDownEvent,
    SkijaRootWidget,
    SkijaBackend[F, OglWindow]
  ](
    backend = downEventSink => SkijaSimpleDrawApi.createForTests(
      windowSize = Size(620, 480),
      windowTitle = "Gui4s window",
      GlfwImpure = ContextFFI(drawLoopExecutionContext, summon),
      CommonImpure = summon,
      onWindowResized = _ => downEventSink.offer(SkijaDownEvent.WindowResized),
      onMouseClick = (button, action, mods) => downEventSink.offer(SkijaDownEvent.MouseClick(button, action, mods)),
      onMouseMove = (x, y) => downEventSink.offer(SkijaDownEvent.MouseMove(x, y)),
      onKeyPress = (key, scancode, action, mods) => downEventSink.offer(SkijaDownEvent.KeyPress(key, scancode, action, mods))
    ),
    drawLoop = backend => runDrawLoopOnExecutionContext[F, Drawable[SkijaDraw[F, OglWindow]]](
      skijaDrawLoop[F, OglWindow](backend),
      drawLoopExecutionContext
    ),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, SkijaRootWidget, SkijaDownEvent](
      updateLoop(handleApplicationRequests),
      updateLoopExecutionContext
    ),
    rootWidget = backend =>
      given RunPlacement[F, MeasurableT[F, Float]] = MeasurableRunPlacement[F, F, Float](backend.windowBounds)

      measurableIsFlatMap[F, Float].map(widget(using backend))(widget =>
        RootWidget[
          F,
          PlacedWidget[F, ApplicationRequest, SkijaDownEvent],
          SkijaDraw[F, OglWindow],
          MeasurableT[F, Float],
          Update[ApplicationRequest],
          Recomposition[F],
          SkijaDownEvent,
        ](
          Path(List("ROOT")),
          widget,
          identity[F[Unit]],
          skijaWidgetHandlesEvent,
          skijaWidgetReactsOnRecomposition,
          skijaWidgetHasInnerStates,
          skijaWidgetIsDrawable
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