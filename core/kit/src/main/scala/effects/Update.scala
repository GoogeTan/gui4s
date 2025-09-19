package gui4s.core.kit
package effects

import catnip.BiMonad
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import cats.*
import cats.data.*
import cats.kernel.Monoid

type UpdateTransformer[State, Events, Error] =
    StateTransformer[State] <> EventsTransformer[Events] <> ErrorTransformer[Error]

given mt [State, Events : Monoid, Error] : MonadTransformer[UpdateTransformer[State, Events, Error]] =
  composedMonadTransformerInstance[StateTransformer[State], EventsTransformer[Events] <> ErrorTransformer[Error]](
    using summon, composedMonadTransformerInstance[
      EventsTransformer[Events], ErrorTransformer[Error]
    ]
  )

type Update[IO[_], State, Events, Error, Value] = UpdateTransformer[State, Events, Error][IO, Value]

object Update:
  given biMonadInstance[IO[_] : Monad, State, Error] : BiMonad[[A, B] =>> UpdateTransformer[State, List[A], Error][IO, B]] =
    [T] => () => monadInstanceForTransformer

  def liftK[IO[_] : Monad, State, Events : Monoid, Error] : IO ~> Update[IO, State, Events, Error, *] =
    mt.liftK
  end liftK

  def raiseError[IO[_] : Monad, State, Events : Monoid, Error, Value](error : Error) : Update[IO, State, Events, Error, Value] =
    ErrorTransformer.raiseError(error)
  end raiseError

  def getState[IO[_] : Monad, State, Events: Monoid, Error]: UpdateTransformer[State, Events, Error][IO, State] =
    StateTransformer.get_
  end getState

  def setState[IO[_] : Monad, State, Events: Monoid, Error](state: State): UpdateTransformer[State, Events, Error][IO, Unit] =
    StateTransformer.set_(state)
  end setState

  def updateState[IO[_] : Monad, State, Events: Monoid, Error](f: State => State): UpdateTransformer[State, Events, Error][IO, Unit] =
    StateTransformer.modify_(f)
  end updateState

  def emitEvents[IO[_] : Monad, State, Events: Monoid, Error](events: Events): UpdateTransformer[State, Events, Error][IO, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def catchEvents[IO[_] : Monad, State, Events : Monoid, NewEvents : Monoid, Error]: [T] => UpdateTransformer[State, Events, Error][IO, T] => UpdateTransformer[State, NewEvents, Error][IO, (T, Events)] =
    [T] => update => EventsTransformer.catchEvents(update)
  end catchEvents

  def mapEvents[IO[_] : Monad, State, Events : Monoid, NewEvents : Monoid, Error](f : Events => NewEvents)
  : UpdateTransformer[State, Events, Error][IO, *] ~> UpdateTransformer[State, NewEvents, Error][IO, *] =
    new ~>[
      UpdateTransformer[State, Events, Error][IO, *], UpdateTransformer[State, NewEvents, Error][IO, *]
    ]:
      override def apply[A](fa: UpdateTransformer[State, Events, Error][IO, A]): UpdateTransformer[State, NewEvents, Error][IO, A] =
        for
          tmp <- catchEvents[IO, State, Events, NewEvents, Error][A](fa)
          (result, events) = tmp
          _ <- emitEvents(f(events))
        yield result
      end apply
    end new
  end mapEvents

  def run[
    IO[_] : Monad,
    State,
    Events : Monoid,
    Error
  ](
    initialState : State,
  ) : [T] => UpdateTransformer[State, Events, Error][IO, T] => IO[Either[Error, (Events, (State,  T))]] =
    [T] => (update : StateT[WriterT[EitherT[IO, Error, *], Events, *], State, T]) =>
      update.run(initialState).run.value
  end run

  def getCornerCoordinates[IO[_] : Monad, Point, Clip, Events : Monoid, Error] : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Point] =
    getState.map(_.widgetCoordinates)
  end getCornerCoordinates

  def getClip[IO[_] : Monad, Point, Clip, Events : Monoid, Error] : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Clip] =
    getState.map(_.path)
  end getClip

  def setClip[IO[_] : Monad, Point, Clip, Events : Monoid, Error](clip : Clip) : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Unit] =
    updateState(_.withClip(clip))
  end setClip

  def markEventHandled[IO[_] : Monad, Point, Clip, Events : Monoid, Error] : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Unit] =
    updateState(_.markEventHandled)
  end markEventHandled

  def isEventHandled[IO[_] : Monad, Point, Clip, Events : Monoid, Error] : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Boolean] =
    getState.map(_.consumed)
  end isEventHandled

  def withCornerCoordinates[
    IO[_] : Monad,
    Point,
    Clip,
    Events : Monoid,
    Error,
    Value
  ](
    original : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Value],
    f : Point => Point
  ) : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      _ <- updateState[IO, UpdateState[Point, Clip], Events, Error](_.withCoordinates(f(coordinates)))
      res <- original
      _ <- updateState[IO, UpdateState[Point, Clip], Events, Error](_.withCoordinates(coordinates))
    yield res
  end withCornerCoordinates

  def withClip[
    IO[_] : Monad,
    Point,
    Clip,
    Events : Monoid,
    Error,
    Value
  ](
    original : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Value],
    f : (Clip, Point) => Clip
  ) : UpdateTransformer[UpdateState[Point, Clip], Events, Error][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      clip <- getClip
      _ <- setClip(f(clip, coordinates))
      res <- original
      _ <- setClip(clip)
    yield res
  end withClip
end Update
