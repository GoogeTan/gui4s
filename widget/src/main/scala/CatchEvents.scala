package me.katze.gui4s.widget

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.StateT
import cats.syntax.all.*

trait CatchEvents[Update[_, _]]:
  extension[Event, Value](old : Update[Event, Value])
    def catchEvents[NewEventType] : Update[NewEventType, (List[Event], Value)]
  end extension
end CatchEvents

// TODO Надо исправить это недорозумение
given liftStateTCatchEvents[F[_, _] : {BiMonad, CatchEvents}, S] : CatchEvents[[Event, Value] =>> StateT[F[Event, *], S, Value]] with
  extension [Event, Value](old: StateT[F[Event, *], S, Value])
    override def catchEvents[NewEventType]: StateT[F[NewEventType, *], S, (List[Event], Value)] =
      val inner : F[Event, S => F[Event, (S, Value)]] = old.runF
      val inner2: F[NewEventType, (List[Event], S => F[Event, (S, Value)])] = inner.catchEvents[NewEventType]
      val inner3 : F[NewEventType, S => F[NewEventType, (S, (List[Event], Value))]] = inner2.map((events : List[Event], f : S => F[Event, (S, Value)]) =>
        (s : S) =>
          f(s).catchEvents[NewEventType].map((events2, v) => (v._1, (events ++ events2, v._2)))
      )
      StateT.applyF(inner3)
    end catchEvents
  end extension
end liftStateTCatchEvents

