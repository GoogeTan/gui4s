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
import me.katze.gui4s.geometry.{Point2d, Rect}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.{widgetHandlesEvent, widgetHasInnerStates, widgetIsDrawable, widgetReactsOnRecomposition}

import scala.annotation.experimental
import scala.concurrent.ExecutionContext
import scala.language.experimental.namedTypeArguments

@experimental
def skijaApp[
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
   createGlfwCallbacks : (DownEvent => F[Unit]) =>  skija.SkijaSimpleDrawApi.GlfwCallbacks[F[Unit], Float]
): F[ExitCode] =
  type SkijaRootWidget = RootWidget[
    F,
    SkijaPlacedWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, DownEvent],
    SkijaDraw[F, GlfwWindow[F, Long, Float]],
    SkijaPlaceT[F, Float, PlaceError],
    SkijaUpdateT[F, UpdateError, Float, ApplicationRequest],
    SkijaRecomposition[F],
    DownEvent,
  ]

  runApplicationLoopsWithBackend[
    F,
    DownEvent,
    SkijaRootWidget,
    SkijaBackend[F, Long, GlfwWindow[F, Long, Float], DownEvent]
  ](
    backend = downEventSink => SkijaSimpleDrawApi.createForTestsTrue(
      queue = downEventSink,
      settings = WindowCreationSettings(
        title = "Gui4s window",
        size = Rect(620, 480),
        visible = true,
        resizeable = false,
        debugContext = true
      ),
      ffi = ContextForeighFunctionInterface(drawLoopExecutionContext, summon),
      callbacks = createGlfwCallbacks(downEventSink.offer)
    ),
    drawLoop = backend =>
      val a : DrawLoop[F, Drawable[SkijaDraw[F, GlfwWindow[F, Long, Float]]]] = skijaDrawLoop[F, Long, GlfwWindow[F, Long, Float], DownEvent, Float](backend)
      runDrawLoopOnExecutionContext[
        F, Drawable[SkijaDraw[F, GlfwWindow[F, Long, Float]]]
      ](a, drawLoopExecutionContext),
    updateLoop = backend => runUpdateLoopOnExecutionContext[F, SkijaRootWidget, DownEvent](
      updateLoop(handleApplicationRequests[F, UpdateError, Float](updateErrorAsExitCode)),
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
            SkijaPlacedWidget[F, Float, UpdateError, PlaceError, ApplicationRequest, DownEvent],
            SkijaDraw[F, GlfwWindow[F, Long, Float]],
            SkijaPlaceT[F, Float, PlaceError],
            SkijaUpdateT[F, UpdateError, Float, ApplicationRequest],
            SkijaRecomposition[F],
            DownEvent,
          ](
            Path(List("ROOT")),
            widget,
            SkijaRecomposition.run[F],
            widgetHandlesEvent[Update = SkijaUpdateT[F, UpdateError, Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetReactsOnRecomposition[Update = SkijaUpdateT[F, UpdateError, Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetHasInnerStates[Update = SkijaUpdateT[F, UpdateError, Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]],
            widgetIsDrawable[Update = SkijaUpdateT[F, UpdateError, Float, ApplicationRequest], Place = SkijaPlaceT[F, Float, PlaceError]]
          )
        )
      )
  )
end skijaApp
