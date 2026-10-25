package gui4s.android.kit.widgets

import scala.concurrent.duration.{Duration, FiniteDuration}

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*

import gui4s.core.widget.library.animation.*

import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.eventCatcher

def loopEach(
            calc : IO[Unit],
            time : FiniteDuration,
) : IO[Unit] =
  (calc *> IO.sleep(time)).iterateWhile(_ => true)
end loopEach

//TODO
def animationWidget[
  AnimatedValue : Eq
] : AnimationWidget[AndroidWidget, AnimatedValue, Duration] =
  gui4s.core.widget.library.animation.animationWidget[
    AndroidWidget,
    Duration,
    Update,
    Place,
    * => RecompositionReaction,
    AnimatedValue
  ](
    statefulWidget =
      [Event] => (
        name,
        initialState,
        handleEvents,
        body,
        mergeStates
      ) =>
        transitiveStatefulWidget[AnimationWidgetState[AnimatedValue, Duration], Event, Duration](
          name,
          initialState.pure,
          _ => None.pure,
          (state, events) => handleEvents(state, events).map(_.pure[Option]),
          body,
          mergeStates
        ),
    currentTime = [T] => callback => PlacementEffect.liftF(Clock[IO].monotonic).flatMap(callback),
    timeSourceWidget = [Event] => original =>
      eventCatcher[Either[Duration, Event]](
        _ =>
          Update
            .liftK[Either[Duration, Event]](Clock[IO].monotonic)
            .map(Left[Duration, Event])
            .map(NonEmptyList.one)
            .flatMap(Update.emitEvents(_))
      )(
        original.mapEvent(Right(_))
      )
  )
end animationWidget
