package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import update.EventConsumer

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget.stateful.Path
import me.katze.gui4s.widget.{Widget, collectQuitCompositionReactions}

final case class RootWidget[
  F[+_] : Monad,
  Draw,
  Place[+_],
  Update[+_] : Monad,
  Recomposition,
  DownEvent,
](
  pathToRoot : Path,
  placedWidget: Widget[Update, Draw, Place, Recomposition, DownEvent],
  runRecomposition : Recomposition => F[Unit]
)(
  using RunPlacement[F, Place]
) extends EventConsumer[Update, F[RootWidget[F, Draw, Place, Update, Recomposition, DownEvent]], DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): Update[F[RootWidget[F, Draw, Place, Update, Recomposition, DownEvent]]] =
    placedWidget.handleDownEvent(pathToRoot, event).map(newWidget =>
      for
        newPlacedWidget <- newWidget.runPlacement
        _ <- runRecomposition(newPlacedWidget.recomposed(pathToRoot, placedWidget.childrenStates))
        _ <- collectQuitCompositionReactions[Recomposition](placedWidget.childrenStates, newPlacedWidget.childrenStates).traverse_(runRecomposition)
      yield copy(placedWidget = newPlacedWidget),
    )
  end processEvent

  override def draw: Draw =
    placedWidget.draw
  end draw
end RootWidget

