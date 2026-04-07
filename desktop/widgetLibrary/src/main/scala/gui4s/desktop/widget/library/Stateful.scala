package gui4s.desktop.widget.library

import catnip.syntax.all._
import cats.Functor
import cats.Monad
import cats.Monoid
import cats.syntax.all._
import cats.~>

import gui4s.core.widget
import gui4s.core.widget._
import gui4s.core.widget.draw.statefulIsDrawable
import gui4s.core.widget.draw.statefulStateDrawsIntoWidget
import gui4s.core.widget.handle._
import gui4s.core.widget.library.MergeStates
import gui4s.core.widget.merge.UpdateWidgetStateFromTheOldOne
import gui4s.core.widget.merge.statefulMergesWithOldStates
import gui4s.core.widget.recomposition.statefulReactsOnRecomposition
import gui4s.core.widget.state.statefulHasInnerStates

/**
 * Виджет с состоянием
 *
 * @tparam State Хранимоге состояние
 * @tparam ChildEvent Порождаемые события дочерних виджетов
 * @tparam Update Эффект обновления виджета.
 * @tparam ChildUpdate Эффект обновления дочернего виджета.
 * @tparam Place Эффект установки виджета на экран.(см. TODO ссылка)
 * @tparam Draw Эффект отрисовки
 * @tparam RecompositionReaction Реакция на рекомпозицию(см. документацию по композиции TODO ссылка).
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
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction : Monoid as M,
  State,
  ChildEvent
](
  widgetsAreMergeable : UpdateWidgetStateFromTheOldOne[
    Place,
    Widget[ChildUpdate, Place, Draw, RecompositionReaction]
  ],
  typeCheckState : [T] => (Any, StatefulState[State] => Option[Place[T]]) => Option[Place[T]],
  liftUpdate : [T] => ChildUpdate[T] => Update[(T, List[ChildEvent])],
  addNameToUpdatePath : String => Update ~> Update,
  addNameToPlacePath : String => Place ~> Place,
)(
  name : String,
  initialState : State,
  handleEvent : HandlesEventF[State, List[ChildEvent], Update * Option],
  render : State => Place[Widget[ChildUpdate, Place, Draw, RecompositionReaction]],
  destructor : State => RecompositionReaction,
  mergeStates : MergeStates[Place, State]
) : Place[
  Widget[
    Update,
    Place,
    Draw,
    RecompositionReaction
  ]
] =
  given Functor[Update * Option] = nestedFunctorsAreFunctors()

  addNameToPlacePath(name)(render(initialState)).map(initialChild =>
    type ChildWidget = Widget[
      ChildUpdate,
      Place,
      Draw,
      RecompositionReaction
    ]
    type StState = StatefulBehaviour[
      State,
      State => Place[ChildWidget],
      HandlesEventF[State, List[ChildEvent], Update * Option],
      State => RecompositionReaction
    ]
    val stateful = Stateful[ChildWidget, StState](
      name = name,
      stateBehaviour = StatefulBehaviour(
        name = name,
        state = StatefulState(
          initialState = initialState,
          currentState = initialState,
        ),
        draw = (state : State) => addNameToPlacePath(name)(render(state)),
        handleEvents = handleEvent,
        destructor = destructor
      ),
      child = initialChild
    )
    val statefulAsFree = widget.free.statefulAsFree[Place, ChildWidget, StState](widgetAsFree)
    Widget.ValueWrapper[
      Stateful[ChildWidget, StState],
      Update,
      Place,
      Draw,
      RecompositionReaction
    ](
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree,
      valueIsDrawable = statefulIsDrawable(widgetIsDrawable),
      valueHandlesEvent = statefulHandlesEvent(
        stateHandlesEvents = statefulStateHandlesEvents,
        drawStateIntoWidget = statefulStateDrawsIntoWidget,
        childWidgetHandlesEvent =
          mapEventHandle_(
            widgetHandlesEvent[ChildUpdate, Place, Draw, RecompositionReaction]
          )(
            _.map(_.map(addNameToPlacePath(name)[ChildWidget](_)))
          ).andThen(liftUpdate(_)),
        widgetsAreMergable = widgetsAreMergeable,
        addNameToPlacePath = addNameToPlacePath,
        addNameToUpdatePath = addNameToUpdatePath,
      ),
      valueMergesWithOldState = statefulMergesWithOldStates[
        Place,
        Stateful[
          ChildWidget,
          StState,
        ],
        StatefulState[State],
        RecompositionReaction
      ](
        typeCheckState = typeCheckState,
        mergeStates = (path, savedState, newState) =>
          mergeStates(
            savedState,
            newState.stateBehaviour.state,
            newStState =>
              widgetsAreMergeable.mergeUpdatedAndRerenderedWidgets(
                path,
                widgetAsFree(newState.child),
                render(newStState.currentState)
              ).map(
                newChild =>
                  newState.copy(
                    stateBehaviour = newState.stateBehaviour.copy(
                      state = newStState
                    ),
                    child = newChild
                  )
              )
          ),
        stateName = _.name,
        widgetStateAsFree = statefulAsFree
      ),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        widgetReactsOnRecomposition[ChildUpdate, Place, Draw, RecompositionReaction]
      ),
      valueHasInnerState = statefulHasInnerStates(widgetHasInnerStates)
    )
  )
end stateful