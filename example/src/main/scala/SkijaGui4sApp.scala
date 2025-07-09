package me.katze.gui4s.example

import catnip.syntax.all.{*, given}
import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import catnip.FFI
import catnip.cats.effect.ContextFFI
import cats.arrow.FunctionK
import cats.data.EitherT
import cats.{Functor, Monad}
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.example.api.exported.{SkijaPlacedWidget, SkijaRecomposition, SkijaPlaceT, SkijaUpdateT, SkijaWidget, handleApplicationRequests, runPlaceLift, runPlaceStateT, skijaInnerRunPlace, skijaRunPlace, given}
import me.katze.gui4s.widget.library.{widgetHandlesEvent, widgetHasInnerStates, widgetIsDrawable, widgetReactsOnRecomposition}
import me.katze.gui4s.example.draw.skija.SkijaSimpleDrawApi.GlfwCallbacks
import me.katze.gui4s.glfw.{KeyAction, KeyModes, OglWindow, Size, WindowCreationSettings}
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.{Path, given}
import me.katze.gui4s.layout.{*, given}
import scala.math.Numeric.Implicits.*

import scala.language.experimental.namedTypeArguments
import scala.concurrent.ExecutionContext

enum SkijaDownEvent:
  case WindowResized
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(x: Double, y: Double)
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset : Double, yoffset : Double)
end SkijaDownEvent

def skijaApp[F[+_] : {Async, Console, FFI}, PlaceError](
                                                          widget: SkijaBackend[F, OglWindow] ?=> SkijaWidget[F, Float, PlaceError, ApplicationRequest, SkijaDownEvent],
                                                          updateLoopExecutionContext: ExecutionContext,
                                                          drawLoopExecutionContext: ExecutionContext,
                                                          runEitherTError : [T] => EitherT[F, PlaceError, T] => F[T]
): F[ExitCode] =
  type SkijaRootWidget[DownEvent] = RootWidget[
    F,
    SkijaPlacedWidget[F, Float, PlaceError, ApplicationRequest, SkijaDownEvent],
    SkijaDraw[F, OglWindow],
    SkijaPlaceT[F, Float, PlaceError],
    SkijaUpdateT[Float, ApplicationRequest],
    SkijaRecomposition[F],
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
        resizeable = false,
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
      updateLoop(handleApplicationRequests[F, Float]),
      updateLoopExecutionContext
    ),
    rootWidget = backend =>
      given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
        [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
          runEitherTError(skijaRunPlace[F, Float, PlaceError](backend.windowBounds)(place))

      runPlacement(
        Functor[SkijaPlaceT[F, Float, PlaceError]].map(widget(using backend))(widget =>
          RootWidget[
            F,
            SkijaPlacedWidget[F, Float, PlaceError, ApplicationRequest, SkijaDownEvent],
            SkijaDraw[F, OglWindow],
            SkijaPlaceT[F, Float, PlaceError],
            SkijaUpdateT[Float, ApplicationRequest],
            SkijaRecomposition[F],
            SkijaDownEvent,
          ](
            Path(List("ROOT")),
            widget,
            identity[F[Unit]],
            widgetHandlesEvent[Update = SkijaUpdateT[Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetReactsOnRecomposition[Update = SkijaUpdateT[Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetHasInnerStates[Update = SkijaUpdateT[Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetIsDrawable[Update = SkijaUpdateT[Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]]
          )
        )
      )
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
