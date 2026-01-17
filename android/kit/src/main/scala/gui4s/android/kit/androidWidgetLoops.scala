package gui4s.android.kit

import scala.concurrent.ExecutionContext
import gui4s.desktop.widget.library.*
import effects.*
import effects.Update.given
import effects.Place.given
import widgets.*
import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import gui4s.core.loop.*
import gui4s.core.widget.Path
import gui4s.core.widget.draw.Drawable

def androidWidgetLoops[
  IO[_] : Async,
  Event,
](
  runUpdate : [T] => Update[IO, Event, T] => IO[Either[ExitCode, T]],
  runPlace : PlaceC[IO] ~> IO,
) : UpdateLoop[IO, AndroidPlacedWidget[IO, Event], DownEvent, ExitCode] =
  updateLoop[IO, AndroidPlacedWidget[IO, Event], DownEvent, ExitCode](
    (widget, event) =>
      flattenRight(
        runUpdate[IO[AndroidPlacedWidget[IO, Event]]](
          processEvent[
            IO,
            AndroidPlacedWidget[IO, Event],
            PlaceC[IO],
            Update[IO, Event, *],
            RecompositionReaction[IO],
            DownEvent
          ](
            Path(Nil),
            RecompositionReaction.run[IO],
            widgetHandlesEvent[
              Update[IO, Event, *],
              PlaceC[IO],
              Draw[IO],
              RecompositionReaction[IO],
              DownEvent
            ],
            widgetReactsOnRecomposition[
              Update[IO, Event, *],
              PlaceC[IO],
              Draw[IO],
              RecompositionReaction[IO],
              DownEvent
            ],
            widgetHasInnerStates[
              Update[IO, Event, *],
              PlaceC[IO],
              Draw[IO],
              RecompositionReaction[IO],
              DownEvent
            ],
            runPlace,
          )(widget, event)
        )
      )
  )
end androidWidgetLoops
