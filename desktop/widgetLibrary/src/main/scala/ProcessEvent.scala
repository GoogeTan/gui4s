package gui4s.decktop.widget.library

import cats.*
import cats.syntax.all.given
import gui4s.core.widget.handle.HandlesEvent
import gui4s.core.widget.recomposition.ReactsOnRecomposition
import gui4s.core.widget.state.HasInnerStates
import gui4s.core.widget.{Path, collectQuitCompositionReactions}

def placeForTheFirstTime[
  IO[_] : Monad,
  Widget,
  Place[_],
  Recomposition,
](
  pathToRoot : Path,
  widget : Place[Widget],
  widgetReactsToRecomposition : ReactsOnRecomposition[Widget, Recomposition],
  runRecomposition : Recomposition => IO[Unit],
  runPlacement: [Value] => Place[Value] => IO[Value]
) : IO[Widget] =

  runPlacement(widget).flatMap(newPlacedWidget =>
    runRecomposition(widgetReactsToRecomposition(newPlacedWidget, pathToRoot, Map())).as(newPlacedWidget)
  )
end placeForTheFirstTime

// tODO rename me
def processEvent[
  IO[_] : Monad,
  Widget,
  Place[_],
  Update[_] : Monad,
  Recomposition,
  DownEvent,
](
    pathToRoot : Path,
    runRecomposition : Recomposition => IO[Unit],
    widgetHandlesEvent : HandlesEvent[Widget, DownEvent, Update[Place[Widget]]],
    widgetReactsToRecomposition : ReactsOnRecomposition[Widget, Recomposition],
    widgetHasInnerState : HasInnerStates[Widget, Recomposition],
    runPlacement: [Value] => Place[Value] => IO[Value]
)(
  placedWidget: Widget,
  event: DownEvent
): Update[IO[Widget]] =
    widgetHandlesEvent(placedWidget, pathToRoot, event).map(newWidget =>
      for
        newPlacedWidget <- runPlacement(newWidget)
        _ <- runRecomposition(widgetReactsToRecomposition(newPlacedWidget, pathToRoot, widgetHasInnerState(placedWidget)))
        _ <- collectQuitCompositionReactions[Recomposition](
          widgetHasInnerState(placedWidget),
          widgetHasInnerState(newPlacedWidget)
        ).traverse_(runRecomposition)
      yield newPlacedWidget
    )
end processEvent
