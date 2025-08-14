package me.katze.gui4s.widget.library

import _root_.me.katze.gui4s.geometry.Rect
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.layout.{*, given}

def freeDrawDecorator[
  Update[_] : Monad as M,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit,
](
  original : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
  toDraw : (Draw, Rect[MeasurementUnit]) => Draw
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  PF.map(
      original
  )(
    _.coflatMap {
      case Sized(placedWidget, size) =>
        final case class DrawDecorator(currentWidget : Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent])
        Widget.ValueWrapper(
          valueToDecorate = DrawDecorator(placedWidget),
          valueAsFree = placed => placed.currentWidget.asFree.map(DrawDecorator(_)),
          valueIsDrawable = self => toDraw(self.currentWidget.draw, size),
          valueHandlesEvent = (self, path, event) =>
            self.currentWidget.handleEvent(path, event).map(_.map(_.mapValue(DrawDecorator(_)))),
          valueMergesWithOldState = (self, path, states) =>
            self.currentWidget.mergeWithOldState(path, states).map(DrawDecorator(_)),
          valueReactsOnRecomposition = (self, path, states) =>
            self.currentWidget.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.currentWidget.innerStates
        )
    }
  )
end freeDrawDecorator