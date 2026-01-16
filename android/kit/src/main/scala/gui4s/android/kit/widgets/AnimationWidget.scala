package gui4s.android.kit.widgets

import gui4s.core.widget.library.animation.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.decorator.eventCatcher

import scala.concurrent.duration.{Duration, FiniteDuration}

def animationWidget[
  IO[_] : {MonadThrow, Clock as ClockIO},
  Event,
  AnimatedValue : {Eq, Group}
]() : AnimationWidget[AndroidWidget[IO, Event], AnimatedValue, Duration] =
  val stateful = transitiveStatefulWidget[IO]
  gui4s.core.widget.library.animation.animationWidget[
    AndroidWidget[IO, *],
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

