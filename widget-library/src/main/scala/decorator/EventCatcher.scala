package me.katze.gui4s.widget.library
package decorator

import decorator.Decorator

import catnip.syntax.additional.*
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.Widget
import me.katze.gui4s.widget.{Path, library}

type EventHandleDecorator[Widget, Update] = Update => Decorator[Widget]

/**
 * Декорирует обновление виджета.
 */
def eventHandleDecorator[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
]: EventHandleDecorator[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]] =>
    WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]]
] =
  decorator => original =>
    original.map(
      placedWidget =>
        final case class HandleDecorator(currentWidget: Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent])
        Widget.ValueWrapper(
          valueToDecorate = HandleDecorator(placedWidget),
          valueAsFree = placed => placed.currentWidget.asFree.map(HandleDecorator(_)),
          valueIsDrawable = _.currentWidget.draw,
          valueHandlesEvent = (self, path, event) =>
            decorator(self.currentWidget.handleEvent)(path, event).map(_.map(HandleDecorator(_))),
          valueMergesWithOldState = (self, path, states) =>
            self.currentWidget.mergeWithOldState(path, states).map(HandleDecorator(_)),
          valueReactsOnRecomposition = (self, path, states) =>
            self.currentWidget.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.currentWidget.innerStates
        )
    )
end eventHandleDecorator

type EventCatcherWithRect[Widget, Update, MeasurableUnit, HandlableEvent] =
  ((Path, RectAtPoint2d[MeasurableUnit], HandlableEvent) => Update) => Decorator[Widget]

def eventCatcherWithWidgetsRect[
  Update[_] : Monad,
  OuterPlace[_] : Functor as OPF,
  Draw : Monoid,
  RecompositionReaction,
  HandleableEvent,
  MeasurableUnit,
](
    markEventHandled : Update[Unit],
    coordinatesOfTheWidget : Update[Point3d[MeasurableUnit]]
) : EventCatcherWithRect[
  OuterPlace[Sized[MeasurableUnit, Widget[Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
  Update[Boolean],
  MeasurableUnit,
  HandleableEvent
] =
  decorator => original =>
    OPF.map(original)(
      sizedWidget =>
        sizedWidget.mapValue(
          placedWidget =>
            final case class EventCatcher(currentWidget: Widget[Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent])
            Widget.ValueWrapper(
              valueToDecorate = EventCatcher(placedWidget),
              valueAsFree = placed => placed.currentWidget.asFree.map(_.mapValue(EventCatcher(_))),
              valueIsDrawable = _.currentWidget.draw,
              valueHandlesEvent = (self, path, event) =>
                coordinatesOfTheWidget.flatMap(point3d =>
                  decorator(path, RectAtPoint2d(sizedWidget.size, point3d.projectToXY), event).ifM(
                    markEventHandled *> self.currentWidget.asFree.map(_.mapValue(EventCatcher(_))).pure[Update],
                    self.currentWidget.handleEvent(path, event).map(_.map(_.mapValue(EventCatcher(_))))
                  )
                ),
              valueMergesWithOldState = (self, path, states) =>
                self.currentWidget.mergeWithOldState(path, states).map(_.mapValue(EventCatcher(_))),
              valueReactsOnRecomposition = (self, path, states) =>
                self.currentWidget.reactOnRecomposition(path, states),
              valueHasInnerState =
                self => self.currentWidget.innerStates
            )
        )
    )
end eventCatcherWithWidgetsRect