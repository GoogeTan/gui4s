package gui4s.core.kit
package effects

import catnip.BiMonad
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import catnip.transformer.ErrorTransformer.monadErrorInstance
import cats.*
import cats.kernel.Monoid

type UpdateTransformer[UpdateError, State, Events] =
    StateTransformer[State] <> EventsTransformer[Events] <> ErrorTransformer[UpdateError]


given [UpdateError, State, Events : Monoid] : MonadTransformer[UpdateTransformer[UpdateError, State, Events]] =
  composedMonadTransformerInstance[StateTransformer[State], EventsTransformer[Events] <> ErrorTransformer[UpdateError]](
    using summon, composedMonadTransformerInstance[EventsTransformer[Events], ErrorTransformer[UpdateError]]
  )

type Update[IO[_], UpdateError, State, Events, Value] = UpdateTransformer[UpdateError, State, Events][IO, Value]

trait UpdateOps[IO[_] : Monad, UpdateError, State]:
  given biMonadInstance : BiMonad[[A, B] =>> UpdateTransformer[UpdateError, State, List[A]][IO, B]] =
    [T] => () => monadInstanceForTransformer
    
  given[Event] : MonadError[Update[IO, UpdateError, State, List[Event], *], UpdateError] =
    monadErrorInstance

  def liftK[Events : Monoid]: IO ~> UpdateTransformer[UpdateError, State, Events][IO, *] =
    MonadTransformer[UpdateTransformer[UpdateError, State, Events]].liftK
  end liftK

  def getState[Events: Monoid]: UpdateTransformer[UpdateError, State, Events][IO, State] =
    StateTransformer.get_
  end getState

  def setState[Events: Monoid](state: State): UpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.set_(state)
  end setState

  def updateState[Events: Monoid](f: State => State): UpdateTransformer[UpdateError, State, Events][IO, Unit] =
    StateTransformer.modify_(f)
  end updateState

  def raiseError[Events: Monoid, A](error: UpdateError): UpdateTransformer[UpdateError, State, Events][IO, A] =
    ErrorTransformer.raiseError(error)
  end raiseError

  def emitEvents[Events: Monoid](events: Events): UpdateTransformer[UpdateError, State, Events][IO, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def catchEvents[Events : Monoid, NewEvents : Monoid]: [T] => UpdateTransformer[UpdateError, State, Events][IO, T] => UpdateTransformer[UpdateError, State, NewEvents][IO, (T, Events)] =
    [T] => update => EventsTransformer.catchEvents(update)
  end catchEvents

  def mapEvents[Events : Monoid, NewEvents : Monoid](f : Events => NewEvents)
  : UpdateTransformer[UpdateError, State, Events][IO, *] ~> UpdateTransformer[UpdateError, State, NewEvents][IO, *] =
    new ~>[
      UpdateTransformer[UpdateError, State, Events][IO, *], UpdateTransformer[UpdateError, State, NewEvents][IO, *]
    ]:
      override def apply[A](fa: UpdateTransformer[UpdateError, State, Events][IO, A]): UpdateTransformer[UpdateError, State, NewEvents][IO, A] =
        for
          tmp <- catchEvents[Events, NewEvents][A](fa)
          (result, events) = tmp
          _ <- emitEvents(f(events))
        yield result
      end apply
    end new
  end mapEvents

  def run[
    Events
  ](
    initialState : State,
  ) : [T] => UpdateTransformer[UpdateError, State, Events][IO, T] => IO[Either[UpdateError, (Events, (State,  T))]] =
    [T] => update =>
      update.run(initialState).run.value
  end run
end UpdateOps

trait UpdateStateOps[IO[_] : Monad, UpdateError, Point, Clip] extends UpdateOps[IO, UpdateError, UpdateState[Point, Clip]]:
  def getCornerCoordinates[Events : Monoid] : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Point] =
    getState.map(_.widgetCoordinates)
  end getCornerCoordinates

  def getClip[Events : Monoid] : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Clip] =
    getState.map(_.path)
  end getClip

  def setClip[Events : Monoid](clip : Clip) : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Unit] =
    updateState(_.withClip(clip))
  end setClip

  def markEventHandled[Events : Monoid] : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Unit] =
    updateState(_.markEventHandled)
  end markEventHandled

  def isEventHandled[Events : Monoid] : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Boolean] =
    getState.map(_.consumed)
  end isEventHandled

  def withCornerCoordinates[
    Events : Monoid,
    Value
  ](
    original : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Value],
    f : Point => Point
  ) : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      _ <- updateState(_.withCoordinates(f(coordinates)))
      res <- original
      _ <- updateState(_.withCoordinates(coordinates))
    yield res
  end withCornerCoordinates

  def withClip[
    Events : Monoid,
    Value
  ](
    original : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Value],
    f : (Clip, Point) => Clip
  ) : UpdateTransformer[UpdateError, UpdateState[Point, Clip], Events][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      clip <- getClip
      _ <- setClip(f(clip, coordinates))
      res <- original
      _ <- setClip(clip)
    yield res
  end withClip
end UpdateStateOps
