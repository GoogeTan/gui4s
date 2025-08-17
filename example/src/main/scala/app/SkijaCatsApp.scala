package me.katze.gui4s.example
package app

import api.effects.{SkijaApplicationRequest, SkijaDownEvent}
import api.effects.SkijaDownEvent.eventOfferingCallbacks
import examples.ClickabeExample.PreInit
import skija.SkijaBackend

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.additional.*
import catnip.syntax.functionk.{eitherTMapError, runEitherT}
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, ExitCode, IO, Resource}
import cats.kernel.Monoid
import cats.~>
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}

import scala.concurrent.ExecutionContext

def skijaGlfwCatsApp[
  Clip : Monoid,
  UpdateError,
  PlaceError,
  HandleableEvent,
  MainThreadPreInit,
](
   preInit : SkijaBackend[IO, Long, OglGlfwWindow, HandleableEvent] => Resource[IO, MainThreadPreInit],
   widget: MainThreadPreInit => SkijaBackend[IO, Long, OglGlfwWindow, HandleableEvent] ?=> SkijaWidget[IO, Float, Clip, UpdateError, PlaceError, SkijaApplicationRequest, HandleableEvent],
   updateLoopExecutionContext: ExecutionContext,
   drawLoopExecutionContext: ExecutionContext,
   updateErrorAsThrowable : UpdateError => Throwable,
   placeErrorAsThrowable  : PlaceError => Throwable,
   createGlfwCallbacks : (HandleableEvent => IO[Unit]) =>  skija.SkijaBackend.GlfwCallbacks[IO[Unit], Float],
   settings : WindowCreationSettings[Float],
): IO[ExitCode] =
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]
  
  skijaGlfwApp[IO, Clip, UpdateError, PlaceError, HandleableEvent, MainThreadPreInit](
    preInit = preInit,
    widget = widget,
    updateLoopExecutionContext = updateLoopExecutionContext,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateErrorAsExitCode = error => IO.println(updateErrorAsThrowable(error)).as(ExitCode.Error),
    runEitherTError = eitherTMapError(placeErrorAsThrowable).andThen(runEitherT[IO, Throwable]),
    createGlfwCallbacks = createGlfwCallbacks,
    settings = settings
  )
