package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import update.EventConsumer

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget.{Path, collectQuitCompositionReactions}
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.draw.Drawable as RawDrawable
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.merge.MergesWithOldStates
import me.katze.gui4s.widget.recomposition.ReactsOnRecomposition
import me.katze.gui4s.widget.state.HasInnerStates

final case class RootWidget[
  F[+_] : Monad,
  Widget,
  Draw,
  Place[+_],
  Update[+_] : Monad,
  Recomposition,
  DownEvent,
](
  pathToRoot : Path,
  placedWidget: Widget,
  runRecomposition : Recomposition => F[Unit],
  widgetHandlesEvent : HandlesEvent[Widget, DownEvent, Update[Place[Widget]]],
  widgetReactsToRecomposition : ReactsOnRecomposition[Widget, Recomposition],
  widgetHasInnerState : HasInnerStates[Widget, Recomposition],
  widgetIsDrawable : RawDrawable[Widget, Draw]
)(
  using runPlacement: RunPlacement[Place, F]
) extends EventConsumer[Update, F[RootWidget[F, Widget, Draw, Place, Update, Recomposition, DownEvent]], DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): Update[F[RootWidget[F, Widget, Draw, Place, Update, Recomposition, DownEvent]]] =
    widgetHandlesEvent(placedWidget, pathToRoot, event).map(newWidget =>
      for
        newPlacedWidget <- runPlacement(newWidget)
        _ <- runRecomposition(widgetReactsToRecomposition(newPlacedWidget, pathToRoot, widgetHasInnerState(placedWidget)))
        _ <- collectQuitCompositionReactions[Recomposition](
          widgetHasInnerState(placedWidget),
          widgetHasInnerState(newPlacedWidget)
        ).traverse_(runRecomposition)
      yield copy(placedWidget = newPlacedWidget),
    )
  end processEvent

  override def draw: Draw =
    widgetIsDrawable(placedWidget)
  end draw
end RootWidget

