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
import cats.effect.{Async, ExitCode, Resource}
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.SkijaDraw
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
  HandleableEvent,
  MainThreadPreInit,
](
    preInit : SkijaBackend[F, Long, OglGlfwWindow, HandleableEvent] => Resource[F, MainThreadPreInit],
    widget: MainThreadPreInit => SkijaBackend[F, Long, OglGlfwWindow, HandleableEvent] ?=> SkijaWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, HandleableEvent],
    updateLoopExecutionContext: ExecutionContext,
    drawLoopExecutionContext: ExecutionContext,
    updateErrorAsExitCode : UpdateError => F[ExitCode],
    runEitherTError : [T] => EitherT[F, PlaceError, T] => F[T],
    createGlfwCallbacks : (HandleableEvent => F[Unit]) =>  skija.SkijaBackend.GlfwCallbacks[F[Unit], Float],
    settings : WindowCreationSettings[Float],
): F[ExitCode] =
  type PlacedWidget = SkijaPlacedWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, HandleableEvent]

  runApplicationLoopsWithBackend[
    F,
    HandleableEvent,
    PlacedWidget,
    (MainThreadPreInit, SkijaBackend[F, Long, OglGlfwWindow, HandleableEvent])
  ](
    backend = downEventSink => 
      for
        skijaBackend <- SkijaBackend.createForTestsTrue(
          queue = downEventSink,
          settings = settings,
          ffi = summon,
          callbacks = createGlfwCallbacks(downEventSink.offer)
        ).evalOn(drawLoopExecutionContext)
        preinit <- preInit(skijaBackend)
      yield (preinit, skijaBackend),
    drawLoop = (_, backend) =>
      runDrawLoopOnExecutionContext(
        skijaDrawLoop[F, PlacedWidget](
          widgetIsDrawable, 
          backend.windowShouldNotClose, 
          backend.drawFrame(ffi, _)
        ),
        drawLoopExecutionContext
      ),
    updateLoop = (_, backend) => runUpdateLoopOnExecutionContext[F, PlacedWidget, HandleableEvent](
      updateLoop(
        (widget, event) =>
          given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
            [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
              runEitherTError(SkijaPlace.run[F, Float, PlaceError](backend.windowBounds)(place))

          SkijaUpdate.handleApplicationRequests[F, Float, UpdateError](updateErrorAsExitCode)(
            processEvent[
              F,
              PlacedWidget,
              SkijaPlaceT[F, Float, PlaceError],
              SkijaUpdateT[F, Float, UpdateError, ApplicationRequest],
              SkijaRecomposition[F],
              HandleableEvent
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
    rootWidget = (preInit, backend) =>
      given runPlacement : RunPlacement[SkijaPlaceT[F, Float, PlaceError], F] =
        [T] => (place : SkijaPlaceT[F, Float, PlaceError][T]) =>
          runEitherTError(SkijaPlace.run[F, Float, PlaceError](backend.windowBounds)(place))
      runPlacement(widget(preInit)(using backend))
  )
end skijaGlfwApp
