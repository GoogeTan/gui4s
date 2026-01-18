package gui4s.core.widget.library.animation

import cats.Applicative
import cats.Eq
import cats.Group
import cats.Order
import cats.data.NonEmptyList
import cats.syntax.all._

import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.MergeStates

type AnimationWidget[Widget, AnimatedValue, Time] =
  (name : String, targetValue : AnimatedValue, animation : Animation[AnimatedValue, Time], body : AnimatedValue => Widget) => Widget

def animationWidget[
  Widget[_],
  Time : {Group, Order},
  Update[_] : Applicative,
  Place[_],
  Destructor[_],
  AnimatedValue : {Group, Eq},
  Event
](
  statefulWidget: (
    name : String,
    initialState: AnimationWidgetState[AnimatedValue, Time],
    handleEvents : HandlesEventF[AnimationWidgetState[AnimatedValue, Time], NonEmptyList[Time], Update],
    body: AnimationWidgetState[AnimatedValue, Time] => Widget[Either[Time, Event]],
    mergeStates : MergeStates[Place, AnimationWidgetState[AnimatedValue, Time]]
  ) => Widget[Event],
  currentTime: [T] => (Time => Place[T]) => Place[T],
  timeSourceWidget : Widget[Event] => Widget[Either[Time, Event]]
) : AnimationWidget[Widget[Event], AnimatedValue, Time] =
  (name, targetValue, animation, body) =>
    statefulWidget(
      name,
      AnimationWidgetState(targetValue, animation, None),
      (state, path, events) => state.withTime(events.maximum).pure[Update],
      state => timeSourceWidget(body(state.valueNow)),
      [T] => (
        oldState : StatefulState[AnimationWidgetState[AnimatedValue, Time]],
        newState: StatefulState[AnimationWidgetState[AnimatedValue, Time]],
        callback : StatefulState[AnimationWidgetState[AnimatedValue, Time]] => Place[T]
      ) =>
        currentTime(
          time =>
            callback(mergeStates(oldState, newState, time))
        )
    )
end animationWidget


def mergeStates[AnimatedValue : {Group as G, Eq as AVEQ}, Time : Group as TG](
  oldState : StatefulState[AnimationWidgetState[AnimatedValue, Time]],
  newState: StatefulState[AnimationWidgetState[AnimatedValue, Time]],
  time : Time
) : StatefulState[AnimationWidgetState[AnimatedValue, Time]] =
  if oldState.initialState == newState.initialState then
    newState
  else
    StatefulState(
      newState.initialState,
      AnimationWidgetState(
        startValue = oldState.currentState.valueAtMoment(time),
        animation = newState.currentState.animation,
        playingAnimation = newState.currentState match
          case AnimationWidgetState(newStartValue, newAnimation, None) =>
            if oldState.currentState.startValue == newStartValue then
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
