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