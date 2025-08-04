package me.katze.gui4s.example
package app

import api.exported.{*, given}
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
import me.katze.gui4s.example.skija.{SkijaBackend, skijaDrawLoop}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.{processEvent, widgetHandlesEvent, widgetHasInnerStates, widgetIsDrawable, widgetReactsOnRecomposition}

import scala.annotation.experimental
import scala.concurrent.ExecutionContext
import scala.language.experimental.namedTypeArguments

@experimental
def skijaGlfwApp[
  F[+_] : {Async as FAsync, Console, ForeighFunctionInterface},
  UpdateError,
  PlaceError,
  DownEvent
](
   widget: SkijaBackend[F, Long, GlfwWindow[F, Long, Float], DownEvent] ?=> SkijaWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, DownEvent],
   updateLoopExecutionContext: ExecutionContext,
   drawLoopExecutionContext: ExecutionContext,
   updateErrorAsExitCode : UpdateError => F[ExitCode],
   runEitherTError : [T] => EitherT[F, PlaceError, T] => F[T],
   createGlfwCallbacks : (DownEvent => F[Unit]) =>  skija.SkijaBackend.GlfwCallbacks[F[Unit], Float],
   settings : WindowCreationSettings[Float],
): F[ExitCode] =
  type PlacedWidget = SkijaPlacedWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, DownEvent]

  runApplicationLoopsWithBackend[
    F,
    DownEvent,
    PlacedWidget,
    SkijaBackend[F, Long, GlfwWindow[F, Long, Float], DownEvent]
  ](
    backend = downEventSink => SkijaBackend.createForTestsTrue(
      queue = downEventSink,
      settings = settings,
      ffi = ContextForeighFunctionInterface(drawLoopExecutionContext, summon),
      callbacks = createGlfwCallbacks(downEventSink.offer)
    ),
    drawLoop = backend =>
      runDrawLoopOnExecutionContext(
        skijaDrawLoop[F, Long, GlfwWindow[F, Long, Float], DownEvent, PlacedWidget](backend, widgetIsDrawable),
        drawLoopExecutionContext
      ),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, PlacedWidget, DownEvent](
      updateLoop(
        (widget, event) =>
          given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
            [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
              runEitherTError(skijaRunPlace[F, Float, PlaceError](backend.windowBounds)(place))

          handleApplicationRequests[F, UpdateError, Float](updateErrorAsExitCode)(
            processEvent[
              F,
              PlacedWidget,
              SkijaPlaceT[F, Float, PlaceError],
              SkijaUpdateT[F, UpdateError, Float, ApplicationRequest],
              SkijaRecomposition[F],
              DownEvent
            ](
              Path(Nil),
              SkijaRecomposition.run[F],
              widgetHandlesEvent,
              widgetReactsOnRecomposition,
              widgetHasInnerStates,
              runPlacement
            )(widget, event)
          )
      ),
      updateLoopExecutionContext
    ),
    rootWidget = backend =>
      given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
        [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
          runEitherTError(skijaRunPlace[F, Float, PlaceError](backend.windowBounds)(place))
      runPlacement(widget(using backend))
  )
end skijaGlfwApp
