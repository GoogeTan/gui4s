package gui4s.core.widget.library.animation

import catnip.BiMonad
import cats.Eq
import cats.Group
import cats.syntax.all.*

import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.MergeStates

/**
 *
 * Виджет, позволяющий плавно интерполировать значение во времени. При изменении target value
 * виджет будет передавать в тело виджета значение, соответствующее текущему моменту времени по правилу animation.
 *
 * @param name Имя состояния анимации
 * @param targetValue Значение, к которому будет плавно интерполировать анимация
 * @param animation Правило, по которому будет интерполироваться анимация
 * @param body Тело виджета
 *
 * @tparam Widget Свободный виджет
 * @tparam AnimatedValue Анимируемое значение
 * @tparam Time Время. На практике будет либо [[Double]], либо [[scala.concurrent.duration.Duration]]
 */
type AnimationWidget[Widget[_], AnimatedValue, Time] =
  [Event] => (name : String, targetValue : AnimatedValue, animation : Animation[AnimatedValue, Time], body : AnimatedValue => Widget[Event]) => Widget[Event]

//TODO Удалить событие для времени .Теперь обработчик виджета состояния вызывается на каждое обновление, даже без событий.
def animationWidget[
  Widget[_],
  Time : {Group, Ordering, Eq},
  Update[_, _] : BiMonad as UBM,
  Place[_],
  Destructor[_],
  AnimatedValue : Eq
](
  statefulWidget: [Event] => (
    name : String,
    initialState: AnimationWidgetState[AnimatedValue, Time],
    handleEvents : HandlesEventF[AnimationWidgetState[AnimatedValue, Time], List[Time], Update[Event, *]],
    body: AnimationWidgetState[AnimatedValue, Time] => Widget[Either[Time, Event]],
    mergeStates : MergeStates[Place, AnimationWidgetState[AnimatedValue, Time]]
  ) => Widget[Event],
  currentTime: [T] => (Time => Place[T]) => Place[T],
  timeSourceWidget : [Event] => Widget[Event] => Widget[Either[Time, Event]]
) : AnimationWidget[Widget, AnimatedValue, Time] =
  [Event] => (name, targetValue, animation, body) =>
    statefulWidget(
      name,
      AnimationWidgetState(targetValue, animation, None),
      (state, events) => UBM().pure(events.maxOption.map(state.withTime).getOrElse(state)),
      state => timeSourceWidget(body(state.valueNow)),
      [T] => (
        oldState : StatefulState[AnimationWidgetState[AnimatedValue, Time]],
        newState: StatefulState[AnimationWidgetState[AnimatedValue, Time]],
        callback : StatefulState[AnimationWidgetState[AnimatedValue, Time]] => Place[T]
      ) =>
        currentTime(
          time =>
            callback(mergeStates(oldState, newState, time))
        ).some
    )
end animationWidget


def mergeStates[AnimatedValue : {Eq as AVEQ}, Time : {Eq, Group as TG}](
  oldState : StatefulState[AnimationWidgetState[AnimatedValue, Time]],
  newState: StatefulState[AnimationWidgetState[AnimatedValue, Time]],
  time : Time
) : StatefulState[AnimationWidgetState[AnimatedValue, Time]] =
  if oldState.initialState === newState.initialState then
    newState
  else
    StatefulState(
      newState.initialState,
      AnimationWidgetState(
        startValue = oldState.currentState.valueAtMoment(time),
        animation = newState.currentState.animation,
        playingAnimation = newState.currentState match
          case AnimationWidgetState(newStartValue, newAnimation, None) =>
            if oldState.currentState.startValue === newStartValue then
              None
            else
              Some(
                AnimationState(
                  targetValue = newStartValue,
                  startVelocity = oldState.initialState.valueAtMoment(time),
                  startTime = time,
                  currentTime = time
                )
              )
          case AnimationWidgetState(newStartValue, newAnimation, Some(newAnimationState)) =>
            Some(
              AnimationState(
                targetValue = newAnimationState.targetValue,
                startVelocity = oldState.initialState.valueAtMoment(time),
                startTime = time,
                currentTime = time
              )
            )
      )
    )
  end if
end mergeStates
