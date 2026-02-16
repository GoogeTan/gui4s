package gui4s.desktop.kit.widgets

import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.{QueueSink, Supervisor}
import cats.syntax.all.*
import gui4s.core.widget.library.animation.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Init.given 
import gui4s.desktop.kit.widgets.decorator.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}

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
          FiniteDuration(15, TimeUnit.MILLISECONDS)
        )
      )
    )
    _ <- Init.emitDecorator(
      eventCatcher[Nothing](
        (_, _, _) =>
          Update.liftK[IO, Nothing](IO.realTime.flatMap(timer.set)).as(false)
      )
    )
  yield {
    val stateful = transitiveStatefulWidget
    gui4s.core.widget.library.animation.animationWidget[
      DesktopWidget,
      Duration,
      Update[IO, Event, *],
      PlaceC[IO],
      * => RecompositionReaction[IO],
      AnimatedValue,
      Event
    ](
      statefulWidget =
        stateful[AnimationWidgetState[AnimatedValue, Duration], Event, Duration](
          _,
          _,
          _,
          _,
          _ => RecompositionReaction.empty,
          _
        ),
      currentTime = [T] => callback => PlacementEffect.liftF(Clock[IO].monotonic).flatMap(callback),
      timeSourceWidget = original =>
        eventCatcher[Either[Duration, Event]](
          (path, _, _) =>
            Update
              .liftK[IO, Either[Duration, Event]](Clock[IO].monotonic)
              .map(Left[Duration, Event])
              .map(NonEmptyList.one)
              .flatMap(Update.emitEvents(_))
              .as(false)
        )(
          original.mapEvent(Right(_))
        )
    )
  }
end animationWidget

