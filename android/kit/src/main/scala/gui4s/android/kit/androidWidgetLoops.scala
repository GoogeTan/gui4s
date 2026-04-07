package gui4s.android.kit

import gui4s.desktop.widget.library.*
import effects.*
import widgets.*
import cats.*
import cats.effect.*
import gui4s.core.loop.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.processEvent

import cats.effect.*
import gui4s.core.loop.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.processEvent
import effects.*
import widgets.*
import cats.*
import cats.effect.IO

def androidWidgetLoops[
  Event,
](
  runUpdate : [T] => (Update[Event, T], List[DownEvent]) => IO[Either[ExitCode, T]],
  runPlace : Place ~> IO,
) : UpdateLoop[IO, AndroidPlacedWidget[Event], List[DownEvent], ExitCode] =
  updateLoop[IO, AndroidPlacedWidget[Event], List[DownEvent], ExitCode](
    (widget, event) =>
      flattenRight(
        runUpdate[IO[AndroidPlacedWidget[Event]]](
          processEvent(
            Path(Nil),
            RecompositionReaction.run[IO],
            widgetAsFree,
            widgetHandlesEvent,
            widgetReactsOnRecomposition,
            widgetHasInnerStates,
            runPlace,
            widget
          ),
          event
        )
      )
  )
end androidWidgetLoops
