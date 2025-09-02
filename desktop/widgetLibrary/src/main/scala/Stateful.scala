package gui4s.desktop.widget.library

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import gui4s.core.widget
import gui4s.core.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import gui4s.core.widget.handle.{HandlesEventF, andThen, statefulHandlesEvent, statefulStateHandlesEvents}
import gui4s.core.widget.merge.{Mergable, statefulMergesWithOldStates}
import gui4s.core.widget.recomposition.statefulReactsOnRecomposition
import gui4s.core.widget.state.statefulHasInnerStates
import gui4s.core.widget.*

/**
 * Виджет с состоянием
 *
 * @tparam State Тип хранимого состояния
 * @tparam ParentEvent Тип порождаемых событий верхнего уровня
 * @tparam ChildEvent Тип порождаемых событий дочерних виджетов
 * @tparam Update Эффект обновления виджета.
 * @tparam Place Эффект установки виджета на экран.(см. TODO ссылка)
 * @tparam Draw Эффект отрисовки
 * @tparam RecompositionReaction Реакция на рекомпозицию(см. документацию по композиции TODO ссылка).
 * @tparam HandlableEvent Тип обрабатываемых внешних/системных событий
 *
 * @param name Имя состояния в дереве состояний(см. дерево состояний TODO ссылка на документацию)
 * @param initialState Начальное состояние
 * @param handleEvent Функция, обрабатывающая события, порожденные дочерними виджетами.
 * @param render Функция отрисовки, отображающая нынешнее состояние в дочернее дерево виджетов.
 * @param destructor Функия отчистки ресурсов. Вызывается, когда виджет покидает композицию.
 * @param widgetsAreMergeable Функция, отвечающая за сохранение старых состояний после создания нового дочернего дерева виджетов.
 * @param typeCheckState Позволяет восстановить стертый тип состояния внутри контекста установки. Имеет такой странный тип, чтобы обойти связанность с внутренней структурой контекста установки
 *
 * Пример использования:
 * {{{
 * stateful(...)(
 *   name = "counter",
 *   initialState = 0,
 *   handleEvent = (state, events) => EventReaction(newState = state + events.length),
 *   render = count => Text(s"Clicked $count times"),
 *   destructor = _ => ()
 * )
 * }}}
 *
 * Виджет содержит в себе состояние и дочернее дерево виджетов, построенное при помощи функции render.
 * Когда дочернее дерево порождает событие, виджет изменяет своё состояние соответсивующим образом в соответсвии с функцией handleEvent.
 * Когда состояние изменилось, строится новое дочерние дерево и оно перенимает состояния из старого дерева при помощи widgetsAreMergable.
 * Если изменилось родительское состояние, слияние происходит по следующему правилу: если initialState изменилось, то берется новое значение, иначе старое.(TODO написать, как деструктор действует в этом случае).
 * Когда виджет покидает композицию, вызывается destructor для отчистки ресурсов, которые могут быть частью состояния.
 */
def stateful[
  Update[_] : Monad,
  ChildUpdate[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  State : Equiv,
  ChildEvent
](
  widgetsAreMergeable : Mergable[Place[Widget[ChildUpdate, Place, Draw, RecompositionReaction, HandlableEvent]]],
  typeCheckState : [T] => (Any, Path, StatefulState[State] => Place[T]) => Place[T],
  liftUpdate : [T] => ChildUpdate[T] => Update[(T, List[ChildEvent])]
)(
  name : String,
  initialState : State,
  handleEvent : HandlesEventF[State, NonEmptyList[ChildEvent], Update],
  render : State => Place[Widget[ChildUpdate, Place, Draw, RecompositionReaction, HandlableEvent]],
  destructor : State => RecompositionReaction,
) : Place[
  Widget[
    Update,
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type ChildWidget = Widget[
      ChildUpdate,
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulBehaviour[
      State,
      State => Place[ChildWidget],
      HandlesEventF[State, NonEmptyList[ChildEvent], Update],
      State => RecompositionReaction
    ]
    val stateful = Stateful(
      name = name,
      stateBehaviour = StatefulBehaviour(
        name = name,
        state = StatefulState(
          initialState = initialState,
          currentState = initialState,
        ),
        draw = render,
        handleEvents = handleEvent,
        destructor = destructor
      ),
      child = initialChild
    )
    val statefulAsFree = widget.free.statefulAsFree[Place, ChildWidget, StState](widgetAsFree)
    Widget.ValueWrapper(
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree,
      valueIsDrawable = statefulIsDrawable(widgetIsDrawable),
      valueHandlesEvent = statefulHandlesEvent(
        stateHandlesEvents = statefulStateHandlesEvents,
        drawStateIntoWidget = statefulStateDrawsIntoWidget,
        childWidgetHandlesEvent = widgetHandlesEvent[ChildUpdate, Place, Draw, RecompositionReaction, HandlableEvent].andThen(liftUpdate(_)),
        widgetsAreMergable = widgetsAreMergeable,
      ),
      valueMergesWithOldState = statefulMergesWithOldStates(typeCheckState, statefulAsFree),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        widgetReactsOnRecomposition[ChildUpdate, Place, Draw, RecompositionReaction, HandlableEvent]
      ),
      valueHasInnerState = statefulHasInnerStates(widgetHasInnerStates)
    )
  )
end stateful