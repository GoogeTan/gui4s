package me.katze.gui4s.example
package api.effects

import catnip.transformer.*
import catnip.syntax.transformer.{*, given}
import cats.*
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s.widget.library.effect.EventsTransformer

type SkijaUpdateTransformer[UpdateError, State, Events] =
  ErrorTransformer[UpdateError] <> StateTransformer[State] <> EventsTransformer[Events]

object SkijaUpdateTransformer:
  def liftK[IO[_] : Monad, UpdateError, State, Events : Monoid]: IO ~> SkijaUpdateTransformer[UpdateError, State, Events][IO, *] =
    MonadTransformer[SkijaUpdateTransformer[UpdateError, State, Events]].liftK
  end liftK

  def getState[IO[_] : Monad, UpdateError, State, Events: Monoid]: SkijaUpdateTransformer[UpdateError, State, Events][IO, State] =
    StateTransformer.get
  end getState

  def setState[IO[_] : Monad, UpdateError, State, Events: Monoid](state: State): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.set(state)
  end setState

  def updateState[IO[_] : Monad, UpdateError, State, Events: Monoid](f: State => State): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.modify(f)
  end updateState

  def raiseError[IO[_] : Monad, UpdateError, State, Events: Monoid, A](error: UpdateError): SkijaUpdateTransformer[UpdateError, State, Events][IO, A] =
    ErrorTransformer.raiseError_(error)
  end raiseError

  def emitEvents[IO[_] : Monad, UpdateError, State, Events: Monoid](events: Events): SkijaUpdateTransformer[UpdateError, State, Events][IO, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def catchEvents[IO[_] : Monad, UpdateError, State, Events : Monoid, NewEvents : Monoid]
      : [T] => SkijaUpdateTransformer[UpdateError, State, Events][IO, T] => SkijaUpdateTransformer[UpdateError, State, NewEvents][IO, (T, Events)] =
    [T] => update => EventsTransformer.catchEvents(update)
  end catchEvents
end SkijaUpdateTransformer
