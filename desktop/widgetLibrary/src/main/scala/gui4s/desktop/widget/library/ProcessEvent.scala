package gui4s.desktop.widget.library

import cats.Monad
import cats.~>

import gui4s.core.widget.Path
import gui4s.core.widget.library.placeForTheFirstTime

final def runWidgetForTheFirstTime[
  IO[_] : Monad,
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
](
  widget: Place[Widget[Update, Place, Draw, RecompositionReaction]],
  runPlace: Place ~> IO,
  runRecompositionReaction: RecompositionReaction => IO[Unit],
): IO[Widget[Update, Place, Draw, RecompositionReaction]] =
  placeForTheFirstTime[
    IO,
    Widget[Update, Place, Draw, RecompositionReaction],
    Place,
    RecompositionReaction
  ](
    Path(Nil),
    widget,
    widgetReactsOnRecomposition[
      Update,
      Place,
      Draw,
      RecompositionReaction
    ],
    runRecompositionReaction,
    runPlace,
  )
end runWidgetForTheFirstTime

def processEvent[
  IO[_] : Monad,
  Update[_] : Monad,
  Place[_],
  Draw,
  RecompositionReaction,
](
  runPlace : Place ~> IO,
  runRecomposition : RecompositionReaction => IO[Unit],
  widget : Widget[Update, Place, Draw, RecompositionReaction],
): Update[IO[Widget[Update, Place, Draw, RecompositionReaction]]] =
  gui4s.core.widget.library.processEvent(
    Path(Nil),
    runRecomposition,
    widgetAsFree,
    widgetHandlesEvent,
    widgetReactsOnRecomposition,
    widgetHasInnerStates,
    runPlace,
    widget
  )
end processEvent
