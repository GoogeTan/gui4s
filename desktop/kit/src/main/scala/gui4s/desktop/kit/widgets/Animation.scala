package gui4s.desktop.kit.widgets

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import cats.*
import cats.syntax.all.*
import cats.data.NonEmptyList
import cats.effect.*

import gui4s.core.widget.library.animation.*

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.widgets.decorator.*

def loopEach(
            calc : IO[Unit],
              time : FiniteDuration,
) : IO[Unit] =
  (calc *> IO.sleep(time)).iterateWhile(_ => true)
end loopEach

def animationWidget[
  AnimatedValue : Eq
] : AnimationWidget[DesktopWidget, AnimatedValue, Duration] =
  gui4s.core.widget.library.animation.animationWidget[
    DesktopWidget,
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
          (state, events) => handleEvents(state, events).map(Some(_)),
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

