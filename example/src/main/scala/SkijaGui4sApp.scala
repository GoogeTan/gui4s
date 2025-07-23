package me.katze.gui4s.example

import api.exported.{*, given}
import draw.*
import draw.skija.*
import place.RunPlacement
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.ContextForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.Functor
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{Pixel, SkijaDraw, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.{widgetHandlesEvent, widgetHasInnerStates, widgetIsDrawable, widgetReactsOnRecomposition}

import scala.annotation.experimental
import scala.concurrent.ExecutionContext
import scala.language.experimental.namedTypeArguments

@SuppressWarnings(Array("org.wartremover.warts.Any"))
enum SkijaDownEvent:
  case WindowResized
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(x: Pixel, y: Pixel)
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset : Pixel, yoffset : Pixel)
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  case TaskRaisedEvent(path : Path, event : Any)
end SkijaDownEvent

@experimental
def skijaApp[
  F[+_] : {Async, Console, ForeighFunctionInterface},
  UpdateError,
  PlaceError,
](
   widget: SkijaBackend[F, OglGlfwWindow[F], Long] ?=> SkijaWidget[F, Pixel, UpdateError, PlaceError, ApplicationRequest, SkijaDownEvent],
   updateLoopExecutionContext: ExecutionContext,
   drawLoopExecutionContext: ExecutionContext,
   updateErrorAsExitCode : UpdateError => F[ExitCode],
   runEitherTError : [T] => EitherT[F, PlaceError, T] => F[T],
): F[ExitCode] =
  type SkijaRootWidget[DownEvent] = RootWidget[
    F,
    SkijaPlacedWidget[F, Pixel, UpdateError, PlaceError, ApplicationRequest, SkijaDownEvent],
    SkijaDraw[F, OglGlfwWindow[F]],
    SkijaPlaceT[F, Pixel, PlaceError],
    SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest],
    SkijaRecomposition[F],
    DownEvent,
  ]

  runApplicationLoopsWithBackend[
    F,
    SkijaDownEvent,
    SkijaRootWidget,
    SkijaBackend[F, OglGlfwWindow[F], Long]
  ](
    backend = downEventSink => SkijaSimpleDrawApi.createForTests(
      queue = downEventSink,
      settings = WindowCreationSettings(
        title = "Gui4s window",
        size = Size(620, 480),
        visible = true,
        resizeable = false,
        debugContext = true
      ),
      ffi = ContextForeighFunctionInterface(drawLoopExecutionContext, summon),
      callbacks = eventOfferingCallbacks(downEventSink.offer)
    ),
    drawLoop = backend => runDrawLoopOnExecutionContext[F, Drawable[SkijaDraw[F, OglGlfwWindow[F]]]](
      skijaDrawLoop[F, OglGlfwWindow[F], Long](backend),
      drawLoopExecutionContext
    ),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, SkijaRootWidget, SkijaDownEvent](
      updateLoop(handleApplicationRequests[F, UpdateError, Pixel](updateErrorAsExitCode)),
      updateLoopExecutionContext
    ),
    rootWidget = backend =>
      given runPlacement : RunPlacement[SkijaPlaceT[F, Pixel, PlaceError], F] =
        [T] => (place : SkijaPlaceT[F, Pixel, PlaceError][T]) =>
          runEitherTError(skijaRunPlace[F, Pixel, PlaceError](backend.windowBounds)(place))

      runPlacement(
        Functor[SkijaPlaceT[F, Pixel, PlaceError]].map(widget(using backend))(widget =>
          RootWidget[
            F,
            SkijaPlacedWidget[F, Pixel, UpdateError, PlaceError, ApplicationRequest, SkijaDownEvent],
            SkijaDraw[F, OglGlfwWindow[F]],
            SkijaPlaceT[F, Pixel, PlaceError],
            SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest],
            SkijaRecomposition[F],
            SkijaDownEvent,
          ](
            Path(List("ROOT")),
            widget,
            identity[F[Unit]],
            widgetHandlesEvent[Update = SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest], Place = SkijaPlaceT[F, Pixel, PlaceError]],
            widgetReactsOnRecomposition[Update = SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest], Place = SkijaPlaceT[F, Pixel, PlaceError]],
            widgetHasInnerStates[Update = SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest], Place = SkijaPlaceT[F, Pixel, PlaceError]],
            widgetIsDrawable[Update = SkijaUpdateT[F, UpdateError, Pixel, ApplicationRequest], Place = SkijaPlaceT[F, Pixel, PlaceError]]
          )
        )
      )
  )
end skijaApp

@experimental
def eventOfferingCallbacks[F](offerEvent : SkijaDownEvent => F) : draw.skija.SkijaSimpleDrawApi.GlfwCallbacks[F] =
  draw.skija.SkijaSimpleDrawApi.GlfwCallbacks(
    onWindowResized = _ => offerEvent(SkijaDownEvent.WindowResized),
    onMouseClick = (button, action, mods) => offerEvent(SkijaDownEvent.MouseClick(button, action, mods)),
    onMouseMove = (x, y) => offerEvent(SkijaDownEvent.MouseMove(Pixel(x.toFloat), Pixel(y.toFloat))),
    onKeyPress = (key, scancode, action, mods) => offerEvent(SkijaDownEvent.KeyPress(key, scancode, action, mods)),
    onScroll = (xoffset, yoffset) => offerEvent(SkijaDownEvent.Scrolled(Pixel(xoffset.toFloat), Pixel(yoffset.toFloat)))
  )
end eventOfferingCallbacks
