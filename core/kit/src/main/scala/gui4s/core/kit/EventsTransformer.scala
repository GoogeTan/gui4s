package gui4s.core.kit

import catnip.syntax.transformer.{_, given}
import catnip.transformer._
import cats.Monad
import cats.kernel.Monoid
import cats.syntax.all._

type EventsTransformer[Events] = WriterTransformer[Events]

object EventsTransformer:
  def raiseEvents_[
    IO[_] : Monad,
    Events : Monoid
  ](
    events: Events
  ): EventsTransformer[Events][IO, Unit] =
    WriterTransformer.tell_(events)
  end raiseEvents_

  def raiseEvents[
    F[_[_], _] : MonadTransformer,
    IO[_] : Monad,
    Events : Monoid
  ](
    events: Events
  ): (F <> EventsTransformer[Events])[IO, Unit] =
    WriterTransformer.tell(events)
  end raiseEvents

  def catchEvents_[
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
    original: EventsTransformer[Events][IO, Value]
  ): EventsTransformer[NewEvents][IO, (Value, Events)] =
    WriterTransformer.extract_[IO, Events, NewEvents, Value](original)
  end catchEvents_

  def catchEvents[
    F[_[_], _] : MonadTransformer,
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
    original: (F <> EventsTransformer[Events])[IO, Value]
  ): (F <> EventsTransformer[NewEvents])[IO, (Value, Events)] =
    WriterTransformer.extract[F, IO, Events, NewEvents, Value](original)
  end catchEvents

  def mapEvents_[
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
     original: EventsTransformer[Events][IO, Value],
     f : Events => NewEvents
  ): EventsTransformer[NewEvents][IO, Value] =
    for
      tmp <- catchEvents_[IO, Events, NewEvents, Value](original)
      (value, events) = tmp
      _ <- raiseEvents_(f(events))
    yield value 
  end mapEvents_
  
  def mapEvents[
    F[_[_], _] : MonadTransformer,
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
     original: (F <> EventsTransformer[Events])[IO, Value],
     f : Events => NewEvents
  ): (F <> EventsTransformer[NewEvents])[IO, Value] =
    for
      tmp <- catchEvents[F, IO, Events, NewEvents, Value](original)
      (value, events) = tmp
      _ <- raiseEvents(f(events))
    yield value  
  end mapEvents
end EventsTransformer
