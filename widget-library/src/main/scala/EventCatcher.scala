package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.{Path, library}
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.Widget

type EventHandleDecorator[Widget, Update] = (Widget, Update) => Widget

/**
 * Декорирует обновление виджета. Полиморфно по отношению к типу состояния.
 */
def eventHandleDecorator_[
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
  (original, decorator) =>
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
end eventHandleDecorator_

type EventCatcherWithRect[Widget, Update, MeasurableUnit, HandlableEvent] =
  Widget => ((Path, RectAtPoint2d[MeasurableUnit], HandlableEvent) => Update) => Widget

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
  original => decorator =>
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