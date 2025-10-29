package gui4s.desktop.kit

import cats.*
import cats.data.*
import cats.effect.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.widgetLoops

import scala.concurrent.ExecutionContext

def desktopWidgetLoops[
  IO[_] : Async,
  CallbackIO[_] : Async,
](
   runDraw : Draw[IO] => IO[Boolean],
   runPlace : PlaceC[IO] ~> IO,
   waitForTheNextEvent : IO[DownEvent],
   drawLoopExecutionContext : ExecutionContext,
   updateLoopExecutionContext : ExecutionContext,
   widget : Ref[IO, DesktopPlacedWidget[IO, ApplicationRequest]],
) : IO[ExitCode] =
  widgetLoops[
    IO,
    CallbackIO,
    UpdateC[IO, ApplicationRequest],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    ExitCode
  ](
    waitForTheNextEvent = waitForTheNextEvent,
    widget = widget,
    runUpdate = Update.handleApplicationRequests(MonadThrow[IO].raiseError),
    runPlace = runPlace,
    runDraw = runDraw,
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext,
    successExitCode = ExitCode.Success
  )
end desktopWidgetLoops