package gui4s.core.kit

import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import cats.Functor
import cats.Monad
import cats.kernel.Monoid
import cats.syntax.all.*

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
    F[_[_], _] : MonadTransformer as FMT,
    Inner[_],
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
    original: (F <> EventsTransformer[Events])[IO, Value]
  )(using InnerTransform[F, Inner], Functor[Inner]): (F <> EventsTransformer[NewEvents])[IO, (Value, Events)] =
    WriterTransformer.extract[F, Inner, IO, Events, NewEvents, Value](original)
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
    F[_[_], _] : MonadTransformer as FMT,
    Inner[_],
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    Value
  ](
     original: (F <> EventsTransformer[Events])[IO, Value],
     f : Events => NewEvents
  )(using InnerTransform[F, Inner], Functor[Inner]): (F <> EventsTransformer[NewEvents])[IO, Value] =
    for
      tmp <- catchEvents[F, Inner, IO, Events, NewEvents, Value](original)
      (value, events) = tmp
      _ <- raiseEvents(f(events))
    yield value  
  end mapEvents
end EventsTransformer
