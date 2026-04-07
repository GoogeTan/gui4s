package gui4s.android.kit.widgets

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{QueueSink, Supervisor}
import cats.syntax.all.*
import gui4s.core.widget.library.animation.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.decorator.eventCatcher

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}
import cats.effect.IO
import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{QueueSink, Supervisor}
import cats.syntax.all.*
import gui4s.core.widget.library.animation.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.decorator.eventCatcher

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}

def loopEach(
            calc : IO[Unit],
            time : FiniteDuration,
) : IO[Unit] =
  (calc *> IO.sleep(time)).iterateWhile(_ => true)
end loopEach

//TODO
def animationWidget[
  Event,
  AnimatedValue : Eq
](
  eventBus : QueueSink[IO, DownEvent]
) : Init[AnimationWidget[AndroidWidget[Event], AnimatedValue, Duration]] =
  for
    supervisor <- Init.evalResource(Supervisor[IO])
    timer : Ref[IO, FiniteDuration] <- Init.eval(Ref.ofEffect(Clock[IO].realTime))
    _ <- 
    Init.eval(
      supervisor.supervise(
        loopEach(
          for
            currentTime <- Clock[IO].realTime
            lastUpdate <- timer.get
            shouldUpdate = (currentTime - lastUpdate) >= FiniteDuration(50, TimeUnit.MILLISECONDS)
            _ <- if shouldUpdate then
              timer.set(currentTime) *> eventBus.offer(DownEvent.WindowShouldBeRedrawn)
            else
              Applicative[IO].unit
          yield (),
          FiniteDuration(15, TimeUnit.MILLISECONDS)
        )
      )
    )
    _ <- Init.emitDecorator(
      eventCatcher[Nothing](
        (_, _) =>
          Update.liftK[Nothing](Clock[IO].realTime.flatMap(timer.set))
      )
    )
  yield {
    val stateful = transitiveStatefulWidget
    gui4s.core.widget.library.animation.animationWidget[
      AndroidWidget,
      Duration,
      Update[Event, *],
      Place,
      * => RecompositionReaction,
      AnimatedValue,
      Event
    ](
      statefulWidget =
        (
          name,
          initialState,
          handleEvents,
          body,
          mergeStates
        ) =>
          stateful[AnimationWidgetState[AnimatedValue, Duration], Event, Duration](
            name,
            initialState,
            (a, b, c) => handleEvents(a, b, c).map(Some(_)),
            body,
            _ => RecompositionReaction.empty,
            mergeStates
          ),
      currentTime = [T] => callback => PlacementEffect.liftF(Clock[IO].monotonic).flatMap(callback),
      timeSourceWidget = original =>
        eventCatcher[Either[Duration, Event]](
          (path, _) =>
            Update
              .liftK[Either[Duration, Event]](Clock[IO].monotonic)
              .map(Left[Duration, Event])
              .map(NonEmptyList.one)
              .flatMap(Update.emitEvents(_))
        )(
          original.mapEvent(Right(_))
        )
    )
  }
end animationWidget

