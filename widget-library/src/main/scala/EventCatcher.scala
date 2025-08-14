package me.katze.gui4s.widget.library

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
](mark : String): EventHandleDecorator[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]] =>
    WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]]
] =
  (original, decorator) =>
    original.map(
      placedWidget =>
        def convert(widget : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]) =
          eventHandleDecorator_[Update, Place, Draw, RecompositionReaction, HandleableEvent](mark)(widget, decorator)
        placedWidget.copy(
          asFree = convert(placedWidget.asFree),
          handleEvent = decorator(placedWidget.handleEvent),
          mergeWithOldState = (path, state) => convert(placedWidget.mergeWithOldState(path, state))
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
    OPF.map(
        original
    )(
      sizedWidget =>
        sizedWidget.mapValue(
          placedWidget =>
            def convert(widget : OuterPlace[Sized[MeasurableUnit, Widget[Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]]) =
              eventCatcherWithWidgetsRect[Update, OuterPlace, Draw, RecompositionReaction, HandleableEvent, MeasurableUnit](markEventHandled, coordinatesOfTheWidget)(widget)(decorator)
            placedWidget.copy(
              asFree = convert(placedWidget.asFree),
              handleEvent = (path, event) =>
                coordinatesOfTheWidget.flatMap(point3d =>
                  decorator(path, RectAtPoint2d(sizedWidget.size, point3d.projectToXY), event).ifM(
                    markEventHandled *> convert(placedWidget.asFree).pure[Update],
                    placedWidget.handleEvent(path, event).map(convert)
                  )
                ),
              mergeWithOldState = (path, oldState) =>
                convert(placedWidget.mergeWithOldState(path, oldState))
            )
        )
    )
end eventCatcherWithWidgetsRect