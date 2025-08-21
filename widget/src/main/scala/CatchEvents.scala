package me.katze.gui4s.widget

import catnip.*
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.syntax.all.*

type CatchEvents[Update[_, _]] = [Event1, Event2, Value] => Update[Event1, Value] => Update[Event2, (List[Event1], Value)]

object CatchEvents:
  def catchEventsWriterT[IO[_] : Monad]: CatchEvents[
    [Event, Value] =>>  WriterT[IO, List[Event], Value]
  ] =
    [E1, E2, V] => update =>
      WriterT.liftF(update.run)
  end catchEventsWriterT


end CatchEvents

