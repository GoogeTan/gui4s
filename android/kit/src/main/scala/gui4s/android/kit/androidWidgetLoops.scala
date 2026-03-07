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
  runUpdate : [T] => Update[Event, T] => IO[Either[ExitCode, T]],
  runPlace : PlaceC ~> IO,
) : UpdateLoop[IO, AndroidPlacedWidget[Event], DownEvent, ExitCode] =
  updateLoop[IO, AndroidPlacedWidget[Event], DownEvent, ExitCode](
    (widget, event) =>
      flattenRight(
        runUpdate[IO[AndroidPlacedWidget[Event]]](
          processEvent[
            IO,
            AndroidPlacedWidget[Event],
            PlaceC,
            Update[Event, *],
            RecompositionReaction,
            DownEvent
          ](
            Path(Nil),
            RecompositionReaction.run[IO],
            widgetAsFree,
            widgetHandlesEvent[
              Update[Event, *],
              PlaceC,
              Draw,
              RecompositionReaction,
              DownEvent
            ],
            widgetReactsOnRecomposition[
              Update[Event, *],
              PlaceC,
              Draw,
              RecompositionReaction,
              DownEvent
            ],
            widgetHasInnerStates[
              Update[Event, *],
              PlaceC,
              Draw,
              RecompositionReaction,
              DownEvent
            ],
            runPlace,
          )(widget, event)
        )
      )
  )
end androidWidgetLoops
