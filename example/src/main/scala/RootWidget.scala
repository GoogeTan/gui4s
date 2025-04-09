package me.katze.gui4s.example

import draw.Drawable
import place.RunPlacement
import task.TaskSet
import update.EventConsumer

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget.stateful.{BiMonad, Path}
import me.katze.gui4s.widget.{EventResult, EventResultP, Widget, collectQuitCompositionReactions}

final case class RootWidget[
  F[+_] : Monad,
  Draw,
  Place[+_],
  Update[+_, +_] : BiMonad,
  Recomposition,
  UpEvent,
  DownEvent,
](
  pathToRoot : Path,
  placedWidget: Widget[Update, Draw, Place, Recomposition, UpEvent, DownEvent],
  runRecomposition : Recomposition => F[Unit]
)(
  using RunPlacement[F, Place]
) extends EventConsumer[Update, F[RootWidget[F, Draw, Place, Update, Recomposition, UpEvent, DownEvent]], UpEvent, DownEvent] with Drawable[Draw]:
  override def processEvent(event: DownEvent): Update[F[RootWidget[F, Draw, Place, Update, Recomposition, UpEvent, DownEvent]], UpEvent] =
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

