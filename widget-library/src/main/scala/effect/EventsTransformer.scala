package me.katze.gui4s.widget.library
package effect

import catnip.syntax.transformer.<>
import catnip.transformer.*
import cats.Monad
import cats.kernel.Monoid
import cats.syntax.all.*
import catnip.syntax.all.{*, given}

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
    T
  ](
    original: EventsTransformer[Events][IO, T]
  ): EventsTransformer[NewEvents][IO, (T, Events)] =
    WriterTransformer.extract_[IO, Events, NewEvents, T](original)
  end catchEvents_

  def catchEvents[
    F[_[_], _] : MonadTransformer,
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    T
  ](
    original: (F <> EventsTransformer[Events])[IO, T]
  ): (F <> EventsTransformer[NewEvents])[IO, (T, Events)] =
    WriterTransformer.extract[F, IO, Events, NewEvents, T](original)
  end catchEvents

  def mapEvents_[
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    T
  ](
    original: EventsTransformer[Events][IO, T],
    f : Events => NewEvents
  ): EventsTransformer[NewEvents][IO, T] =
    for
      tmp <- catchEvents_[IO, Events, NewEvents, T](original)
      (value, events) = tmp
      _ <- raiseEvents_(f(events))
    yield value 
  end mapEvents_
  
  def mapEvents[
    F[_[_], _] : MonadTransformer,
    IO[_] : Monad,
    Events : Monoid,
    NewEvents : Monoid,
    T
  ](
    original: (F <> EventsTransformer[Events])[IO, T],
    f : Events => NewEvents
  ): (F <> EventsTransformer[NewEvents])[IO, T] =
    for
      tmp <- catchEvents[F, IO, Events, NewEvents, T](original)
      (value, events) = tmp
      _ <- raiseEvents(f(events))
    yield value  
  end mapEvents
end EventsTransformer
