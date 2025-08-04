package me.katze.gui4s.widget.library

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates
import me.katze.gui4s.widget.{Path, collectQuitCompositionReactions}

// tODO rename me
def processEvent[
  IO[+_] : Monad,
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
