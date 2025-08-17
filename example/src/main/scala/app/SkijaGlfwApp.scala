package me.katze.gui4s.example
package app

import api.effects.{*, given}
import loop.{runApplicationLoopsWithBackend, runDrawLoopOnExecutionContext, runUpdateLoopOnExecutionContext, updateLoop}
import skija.{SkijaBackend, skijaDrawLoop}

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, ExitCode, Resource}
import cats.kernel.Monoid
import cats.~>
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.glfw.*
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*

import scala.concurrent.ExecutionContext

def skijaGlfwApp[
  F[+_] : {Async as FAsync, Console, ForeighFunctionInterface as ffi},
  Clip : Monoid,
  UpdateError,
  PlaceError,
  HandleableEvent,
  MainThreadPreInit,
](
  preInit : SkijaBackend[F, Long, OglGlfwWindow, HandleableEvent] => Resource[F, MainThreadPreInit],
  widget: MainThreadPreInit => SkijaBackend[F, Long, OglGlfwWindow, HandleableEvent] ?=> SkijaWidget[F, Float, Clip, UpdateError, PlaceError, SkijaApplicationRequest, HandleableEvent],
  updateLoopExecutionContext: ExecutionContext,
  drawLoopExecutionContext: ExecutionContext,
  updateErrorAsExitCode : UpdateError => F[ExitCode],
  runEitherTError : EitherT[F, PlaceError, *] ~> F,
  createGlfwCallbacks : (HandleableEvent => F[Unit]) =>  skija.SkijaBackend.GlfwCallbacks[F[Unit], Float],
  settings : WindowCreationSettings[Float],
): F[ExitCode] =
  type PlacedWidget = SkijaPlacedWidget[F, Float, Clip, UpdateError, PlaceError, SkijaApplicationRequest, HandleableEvent]

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
          SkijaUpdate.handleApplicationRequests[F, Float, Clip, UpdateError](updateErrorAsExitCode)(
            processEvent[
              F,
              PlacedWidget,
              SkijaPlaceT[F, Float, PlaceError],
              SkijaUpdateT[F, Float, Clip, UpdateError, SkijaApplicationRequest],
              SkijaRecomposition[F],
              HandleableEvent
            ](
              Path(Nil),
              SkijaRecomposition.run[F],
              widgetHandlesEvent,
              widgetReactsOnRecomposition,
              widgetHasInnerStates,
              [Value] => (place : SkijaPlace[F, Float, PlaceError, Value]) =>
              runEitherTError(SkijaPlace.run[F, Float, PlaceError](backend.windowBounds)(place))
            )(widget, event)
          )
      ),
      updateLoopExecutionContext
    ),
    rootWidget = (preInit, backend) =>
      placeForTheFirstTime[F, PlacedWidget, SkijaPlaceT[F, Float, PlaceError], SkijaRecomposition[F]](
        Path(Nil),
        widget(preInit)(using backend),
        widgetReactsOnRecomposition,
        SkijaRecomposition.run[F],
        SkijaPlace.run[F, Float, PlaceError](backend.windowBounds).andThen(runEitherTError).convert
      )
  )
end skijaGlfwApp
