package gui4s.desktop.kit.widgets

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import cats._
import cats.data.NonEmptyList
import cats.effect._
import cats.effect.std.QueueSink
import cats.effect.std.Supervisor
import cats.syntax.all._

import gui4s.core.widget.library.animation._

import gui4s.desktop.kit.effects.Init.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.effects.Update.given 
import gui4s.desktop.kit.widgets.decorator._

def loopEach(
            calc : IO[Unit],
              time : FiniteDuration,
) : IO[Unit] =
  (calc *> IO.sleep(time)).iterateWhile(_ => true)
end loopEach

def animationWidget[
  AnimatedValue : Eq
] : AnimationWidget[DesktopWidget, AnimatedValue, Duration] =
  val stateful = transitiveStatefulWidget
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
        stateful[AnimationWidgetState[AnimatedValue, Duration], Event, Duration](
          name,
          initialState,
          (state, events) => handleEvents(state, events).map(Some(_)),
          body,
          _ => RecompositionReaction.empty,
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

