package me.katze.gui4s.widget

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.StateT
import cats.syntax.all.*
import cats.~>

trait CatchEvents[Update[_, _]]:
  extension[Event, Value](old : Update[Event, Value])
    def catchEvents[NewEventType] : Update[NewEventType, (List[Event], Value)]
  end extension
end CatchEvents

given liftStateTCatchEvents[F[_, _] : {BiMonad, CatchEvents}, S] : CatchEvents[[Event, Value] =>> StateT[F[Event, *], S, Value]] with
  extension [Event, Value](old: StateT[F[Event, *], S, Value])
    override def catchEvents[NewEventType]: StateT[F[NewEventType, *], S, (List[Event], Value)] =
      StateT(s =>
        old.run(s).catchEvents[NewEventType].map {
          case (events, (sNew, v)) =>
            (sNew, (events, v))
        }
      )
    end catchEvents
  end extension
end liftStateTCatchEvents

