package me.katze.gui4s.widget

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.Applicative
import cats.data.{EitherT, OptionT, StateT, WriterT}
import cats.syntax.all.*

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

given liftOptionTCatchEvents[F[_, _] : {BiMonad, CatchEvents}] : CatchEvents[[Event, Value] =>> OptionT[F[Event, *], Value]] with
  extension [Event, Value](old: OptionT[F[Event, *], Value])
    override def catchEvents[NewEventType]: OptionT[F[NewEventType, *], (List[Event], Value)] =
      OptionT(
        old.value.catchEvents.map((events, maybeValue) => maybeValue.map(value => (events, value)))
      )
    end catchEvents
  end extension
end liftOptionTCatchEvents

given liftEitherTCatchEvents[F[_, _] : {BiMonad, CatchEvents}, Error] : CatchEvents[[Event, Value] =>> EitherT[F[Event, *], Error, Value]] with
  extension [Event, Value](old: EitherT[F[Event, *], Error, Value])
    override def catchEvents[NewEventType]: EitherT[F[NewEventType, *], Error, (List[Event], Value)] =
      EitherT(
        old.value.catchEvents.map((events, maybeValue) => maybeValue.map(value => (events, value)))
      )
    end catchEvents
  end extension
end liftEitherTCatchEvents

given[F[_] : Applicative]: CatchEvents[[Event, Value] =>> WriterT[F, List[Event], Value]] with
  extension[Event, Value](old : WriterT[F, List[Event], Value])
    override def catchEvents[NewEventType] : WriterT[F, List[NewEventType], (List[Event], Value)] =
      WriterT.liftF(old.run)
    end catchEvents
  end extension
end given