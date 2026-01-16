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
](
  runUpdate : [T] => Update[IO, ApplicationRequest, T] => IO[Either[ExitCode, T]],
  runPlace : PlaceC[IO] ~> IO,
) : UpdateLoop[IO, AndroidPlacedWidget[IO, ApplicationRequest], DownEvent, ExitCode] =
  updateLoop[IO, AndroidPlacedWidget[IO, ApplicationRequest], DownEvent, ExitCode](
    (widget, event) =>
      flattenRight(
        runUpdate[IO[AndroidPlacedWidget[IO, ApplicationRequest]]](
          processEvent[
            IO,
            AndroidPlacedWidget[IO, ApplicationRequest],
            PlaceC[IO],
            Update[IO, ApplicationRequest, *],
            RecompositionReaction[IO],
            DownEvent
          ](
            Path(Nil),
            RecompositionReaction.run[IO],
            widgetHandlesEvent[
              Update[IO, ApplicationRequest, *],
              PlaceC[IO],
              Draw[IO],
              RecompositionReaction[IO],
              DownEvent
            ],
            widgetReactsOnRecomposition[
              Update[IO, ApplicationRequest, *],
              PlaceC[IO],
              Draw[IO],
              RecompositionReaction[IO],
              DownEvent
            ],
            widgetHasInnerStates[
              Update[IO, ApplicationRequest, *],
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
