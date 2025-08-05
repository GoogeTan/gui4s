package me.katze.gui4s.example
package app

import api.exported.{*, given}
import place.RunPlacement
import skija.{SkijaBackend, skijaDrawLoop}
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, ExitCode}
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.given
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*

import scala.annotation.experimental
import scala.concurrent.ExecutionContext
import scala.language.experimental.namedTypeArguments

@experimental
def skijaGlfwApp[
  F[+_] : {Async as FAsync, Console, ForeighFunctionInterface as ffi},
  UpdateError,
  PlaceError,
  DownEvent
](
    widget: SkijaBackend[F, Long, OglGlfwWindow, DownEvent] ?=> SkijaWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, DownEvent],
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
    SkijaBackend[F, Long, OglGlfwWindow, DownEvent]
  ](
    backend = downEventSink => SkijaBackend.createForTestsTrue(
      queue = downEventSink,
      settings = settings,
      ffi = summon,
      callbacks = createGlfwCallbacks(downEventSink.offer)
    ).evalOn(drawLoopExecutionContext),
    drawLoop = backend =>
      given a : backend.windowIsGlfwWindow.type = backend.windowIsGlfwWindow // TODO remove this
      runDrawLoopOnExecutionContext(
        skijaDrawLoop[F, Long, OglGlfwWindow, DownEvent, PlacedWidget](backend, widgetIsDrawable),
        drawLoopExecutionContext
      ),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, PlacedWidget, DownEvent](
      updateLoop(
        (widget, event) =>
          given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
            [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
              runEitherTError(SkijaPlace.run[F, Float, PlaceError](backend.windowBounds)(place))

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
          runEitherTError(SkijaPlace.run[F, Float, PlaceError](backend.windowBounds)(place))
      runPlacement(widget(using backend))
  )
end skijaGlfwApp
