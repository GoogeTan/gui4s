package gui4s.desktop.kit

import scala.concurrent.ExecutionContext

import cats._
import cats.data._
import cats.effect._

import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.widgetLoops

def desktopWidgetLoops[
  IO[_] : Async,
  CallbackIO[_] : Async,
  Event,
](
   runDraw : Draw[IO] => IO[Boolean],
   runPlace : PlaceC[IO] ~> IO,
   waitForTheNextEvent : IO[DownEvent],
   drawLoopExecutionContext : ExecutionContext,
   updateLoopExecutionContext : ExecutionContext,
   widget : Ref[IO, DesktopPlacedWidget[IO, Event]],
) : IO[ExitCode] =
  widgetLoops[
    IO,
    CallbackIO,
    UpdateC[IO, Event],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    ExitCode
  ](
    waitForTheNextEvent = waitForTheNextEvent,
    widget = widget,
    runUpdate = Update.runUpdate[IO, Event],
    runPlace = runPlace,
    runDraw = runDraw,
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext,
    successExitCode = ExitCode.Success
  )
end desktopWidgetLoops