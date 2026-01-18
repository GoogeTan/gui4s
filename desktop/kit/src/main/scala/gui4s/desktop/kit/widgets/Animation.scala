package gui4s.desktop.kit.widgets

import scala.concurrent.duration.Duration

import cats._
import cats.data.NonEmptyList
import cats.effect.kernel.Clock
import cats.syntax.all._

import gui4s.core.widget.library.animation._

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.decorator.eventCatcher

def animationWidget[
  IO[_] : {MonadThrow, Clock as ClockIO},
  Event,
  AnimatedValue : {Eq, Group}
]() : AnimationWidget[DesktopWidget[IO, Event], AnimatedValue, Duration] =
  val stateful = transitiveStatefulWidget[IO]
  gui4s.core.widget.library.animation.animationWidget[
    DesktopWidget[IO, *],
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
    currentTime = [T] => callback => OuterPlace.liftF(ClockIO.monotonic).flatMap(callback),
    timeSourceWidget = original =>
      eventCatcher[IO, Either[Duration, Event]](
        (path, _, _) =>
          Update
            .liftK[IO, Either[Duration, Event]](ClockIO.monotonic)
            .map(Left[Duration, Event])
            .map(NonEmptyList.one)
            .flatMap(Update.emitEvents(_))
            .as(false)
      )(
        original.mapEvent(Right(_))
      )
  )
end animationWidget

