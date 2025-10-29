package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all.*
import cats.syntax.all.*
import cats.{Comonad, Functor}
import gui4s.core.widget.Path
import gui4s.core.widget.handle.HandlesEventF
import gui4s.desktop.widget.library.decorator.Decorator


type UpdateDecorator[Update[_], OuterPlace[_], Widget, HandleableEvent] = HandlesEventF[Widget, HandleableEvent, Update * OuterPlace] => Decorator[OuterPlace[Widget]]
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
  updateDecoratorWithRect[
    Update,
    Place,
    [Value] =>> Value,
    Draw,
    RecompositionReaction,
    HandleableEvent
  ](
    (self : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent], path : Path, event : HandleableEvent) =>
      decorator(self.handleEvent(_, _))(path, event)
  )
end updateDecorator


def updateDecoratorWithRect[
  Update[_] : Functor as UF,
  OuterPlace[_] : Functor as PF,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : UpdateDecorator[
  Update,
  OuterPlace,
  InnerPlace[Widget[Update, [Value] =>> OuterPlace[InnerPlace[Value]], Draw, RecompositionReaction, HandleableEvent]],
  HandleableEvent
] =
  given Functor[OuterPlace * InnerPlace] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]
  decorator => original => PF.map(original)(_.coflatMap(
      sizedWidget =>
        Widget.ValueWrapper[
          InnerPlace[Widget[Update, [Value] =>> OuterPlace[InnerPlace[Value]], Draw, RecompositionReaction, HandleableEvent]],
          Update,
          [Value] =>> OuterPlace[InnerPlace[Value]],
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
        valueToDecorate = sizedWidget,
        valueAsFree = self => PF.map(self.extract.asFree)(_.coflatten),
        valueIsDrawable = _.extract.draw,
        valueHandlesEvent = (self, path, event) => UF.map(decorator(self, path, event))(PF.map(_)(_.coflatten)),
        valueMergesWithOldState = (self, path, event) => PF.map(self.extract.mergeWithOldState(path, event))(_.coflatten),
        valueReactsOnRecomposition = _.extract.reactOnRecomposition(_, _),
        valueHasInnerState = _.extract.innerStates
      )
    )
  )
end updateDecoratorWithRect
