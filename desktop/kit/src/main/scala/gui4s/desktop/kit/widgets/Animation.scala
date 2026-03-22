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
import gui4s.desktop.kit.widgets.decorator._

def loopEach(
            calc : IO[Unit],
              time : FiniteDuration,
) : IO[Unit] =
  (calc *> IO.sleep(time)).iterateWhile(_ => true)
end loopEach

def animationWidget[
  Event,
  AnimatedValue : Eq
](
  eventBus : QueueSink[IO, DownEvent]
) : Init[AnimationWidget[DesktopWidget[Event], AnimatedValue, Duration]] =
  for
    supervisor <- Init.evalResource(Supervisor[IO])
    /*
    timer : Ref[IO, FiniteDuration] <- Init.eval(Ref.ofEffect(IO.realTime))
    _ <- 
    Init.eval(
      supervisor.supervise(
        loopEach(
          for
            currentTime <- IO.realTime
            lastUpdate <- timer.get
            shouldUpdate = (currentTime - lastUpdate) >= FiniteDuration(15, TimeUnit.MILLISECONDS)
            _ <- if shouldUpdate then
              timer.set(currentTime) *> eventBus.offer(DownEvent.WindowShouldBeRedrawn)
            else
              IO.unit
          yield (),
          FiniteDuration(2, TimeUnit.MILLISECONDS)
        )
      )
    )
    _ <- Init.emitDecorator(
      eventCatcher[Nothing](
        (_, _, _) =>
          Update.liftK[Nothing](IO.realTime.flatMap(timer.set))
      )
    )*/
  yield {
    val stateful = transitiveStatefulWidget
    gui4s.core.widget.library.animation.animationWidget[
      DesktopWidget,
      Duration,
      UpdateC[Event],
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

