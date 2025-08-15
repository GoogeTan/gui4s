package me.katze.gui4s.widget.library
package decorator

import decorator.Decorator

import catnip.syntax.all.{*, given}
import cats.{Comonad, Functor}
import cats.syntax.all.*
import me.katze.gui4s.widget.Path

/**
 * Декорирует обновление виджета.
 */
def updateDecorator[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  decorator : Decorator[WidgetHandlesEvent[HandleableEvent, Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]]]
): Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]] =
  _.map(
    placedWidget =>
      Widget.ValueWrapper(
        valueToDecorate = placedWidget,
        valueAsFree = widgetAsFree,
        valueIsDrawable = widgetIsDrawable,
        valueHandlesEvent = (self, path, event) => decorator(self.handleEvent)(path, event),
        valueMergesWithOldState = widgetMergesWithOldState,
        valueReactsOnRecomposition = widgetReactsOnRecomposition,
        valueHasInnerState = widgetHasInnerStates
      )
  )
end updateDecorator

def updateDecoratorWithRect[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    decorator : (
      InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]],
      Path,
      HandleableEvent
    ) => Update[Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]]
): Decorator[Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]] =
  PF.map(_)(_.coflatMap(
      sizedWidget =>
        Widget.ValueWrapper[
          InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]],
          Update,
          Place * InnerPlace,
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
        valueToDecorate = sizedWidget,
        valueAsFree = self => PF.map(self.extract.asFree)(_.coflatten),
        valueIsDrawable = _.extract.draw,
        valueHandlesEvent = (self, path, event) => decorator(self, path, event).map(PF.map(_)(_.coflatten)),
        valueMergesWithOldState = (self, path, event) => PF.map(self.extract.mergeWithOldState(path, event))(_.coflatten),
        valueReactsOnRecomposition = _.extract.reactOnRecomposition(_, _),
        valueHasInnerState = _.extract.innerStates
      )
    )
  )
end updateDecoratorWithRect
