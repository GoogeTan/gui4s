package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Eq, Functor, Monoid}
import me.katze.gui4s.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import me.katze.gui4s.widget.free.statefulAsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, andThen, statefulHandlesEvent, statefulStateHandlesEvents}
import me.katze.gui4s.widget.merge.{Mergable, statefulMergesWithOldStates}
import me.katze.gui4s.widget.recomposition.statefulReactsOnRecomposition
import me.katze.gui4s.widget.state.statefulHasInnerStates
import me.katze.gui4s.widget.{CatchEvents, Path, Stateful, StatefulBehaviour, StatefulState}

import scala.language.experimental.namedTypeArguments

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
 * @tparam EventReaction Тип реакции на события. Обычно кодирует эффект, происходящий при обновлении состояния.
 *
 * @param name Имя состояния в дереве состояний(см. дерево состояний TODO ссылка на документацию)
 * @param initialState Начальное состояние
 * @param handleEvent Функция, обрабатывающая события, порожденные дочерними виджетами.
 * @param render Функция отрисовки, отображающая нынешнее состояние в дочернее дерево виджетов.
 * @param destructor Функия отчистки ресурсов. Вызывается, когда виджет покидает композицию.
 * @param widgetsAreMergeable Функция, отвечающая за сохранение старых состояний после создания нового дочернего дерева виджетов.
 * @param runEventReaction Запускает реакцию на событие в контексте обновления. TODO может стоит вообще избавиться от реакции и напрямую конструировать Update[State]?
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
  Update[Event, Value] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  EventReaction,
  State : Equiv,
  ParentEvent,
  ChildEvent
](
   widgetsAreMergeable : Mergable[Place[Widget[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]]],
   runEventReaction : (EventReaction, Path) => Update[ParentEvent, State],
   typeCheckState : [T] => (Any, Path, StatefulState[State] => Place[T]) => Place[T],
)(
   name : String,
   initialState : State,
   handleEvent : (State, NonEmptyList[ChildEvent]) => EventReaction,
   render : State => Place[Widget[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
   destructor : State => RecompositionReaction,
) : Place[
  Widget.ValueWrapper[
    Stateful[
      Widget[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
      StatefulBehaviour[
        State,
        State => Place[Widget[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
        (State, Path, NonEmptyList[ChildEvent]) => Update[ParentEvent, State],
        State => RecompositionReaction
      ],
    ],
    Update[ParentEvent, *],
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type Widget_[E] = Widget[
      Update[E, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulBehaviour[
      State,
      State => Place[Widget_[ChildEvent]],
      (State, Path, NonEmptyList[ChildEvent]) => Update[ParentEvent, State],
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
        handleEvents =
          (state : State, path : Path, events : NonEmptyList[ChildEvent]) =>
            runEventReaction(handleEvent(state, events), path),
        destructor = destructor
      ),
      child = initialChild
    )
    val statefulAsFree_ = statefulAsFree[Place, Widget_[ChildEvent], StState](widgetAsFree)
    Widget.ValueWrapper[
      T = Stateful[Widget_[ChildEvent], StState],
      Update_ = Update[ParentEvent, *],
      Place_ = Place
    ](
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree_, // TODO Rename me
      valueIsDrawable = statefulIsDrawable(widgetIsDrawable),
      valueHandlesEvent = statefulHandlesEvent_(widgetsAreMergeable),
      valueMergesWithOldState = statefulMergesWithOldStates(typeCheckState, statefulAsFree_),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        widgetReactsOnRecomposition[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
        M.empty
      ),
      valueHasInnerState = statefulHasInnerStates(widgetHasInnerStates)
    )
  )
end stateful

type HandlesEventPlace[Place[_], T, HandlableEvent] = HandlesEvent[T, HandlableEvent, Place[T]]

def statefulHandlesEvent_[
  Update[_, _] : {BiMonad as updateIsBiMonad, CatchEvents},
  Place[_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  State : Equiv,
  ParentEvent,
  ChildEvent
](
   widgetsAreMergeable : Mergable[Place[Widget[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]]],
): HandlesEventPlace[
  [T] =>> Update[ParentEvent, Place[T]],
  Stateful[
    Widget[
      Update[ChildEvent, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ],
    StatefulBehaviour[
      State,
      State => Place[
        Widget[
          Update[ChildEvent, *],
          Place, Draw, RecompositionReaction, HandlableEvent
        ]
      ],
      HandlesEvent[State, NonEmptyList[ChildEvent], Update[ParentEvent, State]],
      State => RecompositionReaction]
  ],
  HandlableEvent,
] = statefulHandlesEvent(using updateIsBiMonad())(
  stateHandlesEvents = statefulStateHandlesEvents[Update = Update[ParentEvent, *]],
  drawStateIntoWidget = statefulStateDrawsIntoWidget,
  childWidgetHandlesEvent = widgetHandlesEvent[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent].andThen(_.catchEvents),
  widgetsAreMergable = widgetsAreMergeable,
)