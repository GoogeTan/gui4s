package gui4s.desktop.widget.library

import cats.{~>, Monad}
import gui4s.core.widget.Path
import gui4s.core.widget.library.placeForTheFirstTime

final def runWidgetForTheFirstTime[
  IO[_] : Monad,
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  DownEvent,
](
  widget: Place[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
  runPlace: Place ~> IO,
  runRecompositionReaction: RecompositionReaction => IO[Unit],
): IO[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]] =
  placeForTheFirstTime[
    IO,
    Widget[Update, Place, Draw, RecompositionReaction, DownEvent],
    Place,
    RecompositionReaction
  ](
    Path(Nil),
    widget,
    widgetReactsOnRecomposition[
      Update,
      Place,
      Draw,
      RecompositionReaction,
      DownEvent,
    ],
    runRecompositionReaction,
    runPlace,
  )
end runWidgetForTheFirstTime