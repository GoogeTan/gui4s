package me.katze.gui4s.example
package api

import catnip.syntax.all.{*, given}
import cats.{Functor, Monad}
import cats.syntax.all.*
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.Widget

def clipWidget[
  Update[_] : Monad as UM,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  Shape,
](
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw
)(
    freeWidgetToClip : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
    shapeFabric : Rect[MeasurementUnit] => Shape
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  PF.map(
    freeWidgetToClip
  )(
    _.coflatMap {
      case Sized(placedWidget, size) =>
        val shape = shapeFabric(size)
        final case class ClipWidget(currentWidget: Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent], shape: Shape)
        Widget.ValueWrapper[
          ClipWidget,
          Update,
          Place * Sized[MeasurementUnit, *],
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
          valueToDecorate = ClipWidget(placedWidget, shape),
          valueAsFree = self => PF.map(self.currentWidget.asFree)(_.coflatMap { case Sized(newWidget, newSize) => ClipWidget(newWidget, shapeFabric(newSize)) }),
          valueIsDrawable = self => drawModifier(self.shape, self.currentWidget.draw),
          valueHandlesEvent = (self, path, event) =>
            withClip[Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]](self.shape, self.currentWidget.handleEvent(path, event))
              .map(_.map(_.coflatMap { case Sized(newWidget, newSize) => ClipWidget(newWidget, shapeFabric(newSize)) })),
          valueMergesWithOldState = (self, path, states) =>
            PF.map(self.currentWidget.mergeWithOldState(path, states))(_.coflatMap { case Sized(newWidget, newSize) => ClipWidget(newWidget, shapeFabric(newSize)) }),
          valueReactsOnRecomposition = (self, path, states) =>
            self.currentWidget.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.currentWidget.innerStates
        )
    }
  )
end clipWidget