package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all._
import cats.{Comonad, Id, Functor}
import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.decorator.Decorator

/**
 * Виджет, позволяющий перегрузить обновление дочернего виджета.
 * Принимает функцию, обрабатывающую обновления вместо оригинальной.
 * Надо учесть, что вызвать оригинальное обновление, если это необходимо - задача пользователя данного виджета.
 * Это полезно, если событие надо поглотить и не пропускать дальше(например, при обработке нажатия мыши).
 *
 * @tparam Update Эффект обновления виджета
 * @tparam Place Эффект установки на экран
 * @tparam Widget Размещенный виджет
 * @tparam HandleableEvent Внешнее событие
 */
type UpdateDecorator[Update[_], Place[_], Widget, HandleableEvent] =
  HandlesEventF[Widget, HandleableEvent, Update * Place] => Decorator[Place[Widget]]

/**
 * Декоратор, позволяющий задекорировать обновление виджета. Отличается от [[updateDecoratorWithRect]] тем, что
 * декоратор принимает в себя функцию обновления оригинального виджета, а не его самого с его контекстом установки.
 */
def updateDecorator[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  decorator : Decorator[
    WidgetHandlesEvent[
      HandleableEvent,
      Update[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]]
    ]
  ]
): Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]] =
  updateDecoratorWithRect[
    Update,
    Place,
    Id,
    Draw,
    RecompositionReaction,
    HandleableEvent
  ](
    (self : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent], path : Path, event : HandleableEvent) =>
      decorator(self.handleEvent(_, _))(path, event)
  )
end updateDecorator

/**
 * Реализация [[UpdateDecorator]]
 */
def updateDecoratorWithRect[
  Update[_] : Functor as UF,
  PlacementEffect[_] : Functor as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : UpdateDecorator[
  Update,
  PlacementEffect,
  Situated[Widget[Update, [Value] =>> PlacementEffect[Situated[Value]], Draw, RecompositionReaction, HandleableEvent]],
  HandleableEvent
] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  decorator => original => PF.map(original)(_.coflatMap(
      sizedWidget =>
        Widget.ValueWrapper[
          Situated[Widget[Update, [Value] =>> PlacementEffect[Situated[Value]], Draw, RecompositionReaction, HandleableEvent]],
          Update,
          PlacementEffect * Situated,
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
