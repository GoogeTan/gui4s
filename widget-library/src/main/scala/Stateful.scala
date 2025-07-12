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
import me.katze.gui4s.widget.{CatchEvents, Path, Stateful, StatefulState}

import scala.language.experimental.namedTypeArguments

/**
 * Виджет с состоянием
 *
 * @tparam State Тип состояния
 * @tparam ParentEvent Тип порождаемых событий верхнего уровня
 * @tparam ChildEvent Тип порождаемых событий дочерних виджетов
 * @tparam Update Контекст обновления виджета.
 * @tparam Place Контекст установки виджета на экран.(см. TODO ссылка)
 * @tparam Draw Контекст отрисовки
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
 * TODO подробное описание контрактов
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
    widgetsAreMergeable : Mergable[Place[Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]]],
    runEventReaction : (EventReaction, Path) => Update[ParentEvent, State],
    typeCheckState : [T] => (Any, Path, (State, State) => Place[T]) => Place[T],
)(
    name : String,
    initialState : State,
    handleEvent : (State, NonEmptyList[ChildEvent]) => EventReaction,
    render : State => Place[Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
    destructor : State => RecompositionReaction,
) : Place[
  Widget_[
    Update[ParentEvent, *],
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type Widget[E] = Widget_[
      Update[E, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulState[
      State,
      State => Place[Widget[ChildEvent]],
      (State, Path, NonEmptyList[ChildEvent]) => Update[ParentEvent, State],
      State => RecompositionReaction
    ]
    val stateful = Stateful(
      name = name,
      state = StatefulState(
        name = name,
        initialState = initialState,
        currentState = initialState,
        draw = render,
        handleEvents =
          (state : State, path : Path, events : NonEmptyList[ChildEvent]) =>
            runEventReaction(handleEvent(state, events), path),
        destructor = destructor
      ),
      child = initialChild
    )
    val statefulAsFree_ = statefulAsFree[Place, Widget[ChildEvent], StState](widgetAsFree)
    Widget[
      T = Stateful[Widget[ChildEvent], StState],
      Update = Update[ParentEvent, *],
      Place = Place
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
  widgetsAreMergeable : Mergable[Place[Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]]],
): HandlesEventPlace[
  [T] =>> Update[ParentEvent, Place[T]],
  Stateful[
    Widget_[
      Update[ChildEvent, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ],
    StatefulState[
      State,
      State => Place[
        Widget_[
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