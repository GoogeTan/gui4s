package me.katze.gui4s.example

import catnip.syntax.all.{*, given}
import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import catnip.FFI
import catnip.cats.effect.ContextFFI
import cats.Monad
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import cats.syntax.all.*
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.example.api.exported.{PlacedWidget, Recomposition, UpdateT, Widget, handleApplicationRequests, given}
import me.katze.gui4s.example.api.{skijaWidgetHandlesEvent, skijaWidgetHasInnerStates, skijaWidgetIsDrawable, skijaWidgetReactsOnRecomposition}
import me.katze.gui4s.example.draw.skija.SkijaSimpleDrawApi.GlfwCallbacks
import me.katze.gui4s.glfw.{KeyAction, KeyModes, OglWindow, Size, WindowCreationSettings}
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.{Path, given}

import scala.language.experimental.namedTypeArguments
import scala.concurrent.ExecutionContext

enum SkijaDownEvent:
  case WindowResized
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(x: Double, y: Double)
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset : Double, yoffset : Double)
end SkijaDownEvent

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
    UpdateT[ApplicationRequest],
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
      settings = WindowCreationSettings(
        title = "Gui4s window",
        size = Size(620, 480),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
      ffi = ContextFFI(drawLoopExecutionContext, summon),
      callbacks = eventOfferingCallbacks(downEventSink.offer)
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

      measurableIsFunctor[F, Float].map(widget(using backend))(widget =>
        RootWidget[
          F,
          PlacedWidget[F, ApplicationRequest, SkijaDownEvent],
          SkijaDraw[F, OglWindow],
          MeasurableT[F, Float],
          UpdateT[ApplicationRequest],
          Recomposition[F],
          SkijaDownEvent,
        ](
          Path(List("ROOT")),
          widget,
          identity[F[Unit]],
          skijaWidgetHandlesEvent[Update = UpdateT[ApplicationRequest]],
          skijaWidgetReactsOnRecomposition[Update = UpdateT[ApplicationRequest]],
          skijaWidgetHasInnerStates[Update = UpdateT[ApplicationRequest]],
          skijaWidgetIsDrawable[Update = UpdateT[ApplicationRequest]]
        )
      ).runPlacement
  )
end skijaApp

def eventOfferingCallbacks[F](offerEvent : SkijaDownEvent => F) : GlfwCallbacks[F] =
  GlfwCallbacks(
    onWindowResized = _ => offerEvent(SkijaDownEvent.WindowResized),
    onMouseClick = (button, action, mods) => offerEvent(SkijaDownEvent.MouseClick(button, action, mods)),
    onMouseMove = (x, y) => offerEvent(SkijaDownEvent.MouseMove(x, y)),
    onKeyPress = (key, scancode, action, mods) => offerEvent(SkijaDownEvent.KeyPress(key, scancode, action, mods)),
    onScroll = (xoffset, yoffset) => offerEvent(SkijaDownEvent.Scrolled(xoffset, yoffset))
  )
end eventOfferingCallbacks
