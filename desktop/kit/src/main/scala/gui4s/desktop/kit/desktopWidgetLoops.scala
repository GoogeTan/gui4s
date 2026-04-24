package gui4s.desktop.kit

import scala.concurrent.ExecutionContext

import cats.*
import cats.data.*
import cats.effect.*

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.widgetLoops

def desktopWidgetLoops[
  Event,
](
   runDraw : Draw => IO[Boolean],
   runPlace : Place ~> IO,
   waitForTheNextEvent : IO[List[DownEvent]],
   drawLoopExecutionContext : ExecutionContext,
   updateLoopExecutionContext : ExecutionContext,
   widget : Ref[IO, DesktopPlacedWidget[Event]],
) : IO[ExitCode] =
  widgetLoops[
    IO,
    IO,
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    List[DownEvent],
    ExitCode
  ](
    waitForTheNextEvent = waitForTheNextEvent,
    widget = widget,
    runUpdate = Update.runUpdate[Event],
    runPlace = runPlace,
    runDraw = runDraw,
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext,
    successExitCode = ExitCode.Success
  )
end desktopWidgetLoops