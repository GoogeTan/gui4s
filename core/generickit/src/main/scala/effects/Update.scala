package gui4s.core.kit
package effects

import catnip.BiMonad
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import cats.*
import cats.kernel.Monoid

type UpdateTransformer[State, Events] =
    StateTransformer[State] <> EventsTransformer[Events]

given [State, Events : Monoid] : MonadTransformer[UpdateTransformer[State, Events]] =
  composedMonadTransformerInstance[StateTransformer[State], EventsTransformer[Events]]

type Update[IO[_], State, Events, Value] = UpdateTransformer[State, Events][IO, Value]

trait UpdateOps[IO[_] : Monad, State]:
  given biMonadInstance : BiMonad[[A, B] =>> UpdateTransformer[State, List[A]][IO, B]] =
    [T] => () => monadInstanceForTransformer


  def getState[Events: Monoid]: UpdateTransformer[State, Events][IO, State] =
    StateTransformer.get_
  end getState

  def setState[Events: Monoid](state: State): UpdateTransformer[State, Events][IO, Unit] =
    StateTransformer.set_(state)
  end setState

  def updateState[Events: Monoid](f: State => State): UpdateTransformer[State, Events][IO, Unit] =
    StateTransformer.modify_(f)
  end updateState

  def emitEvents[Events: Monoid](events: Events): UpdateTransformer[State, Events][IO, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def catchEvents[Events : Monoid, NewEvents : Monoid]: [T] => UpdateTransformer[State, Events][IO, T] => UpdateTransformer[State, NewEvents][IO, (T, Events)] =
    [T] => update => EventsTransformer.catchEvents(update)
  end catchEvents

  def mapEvents[Events : Monoid, NewEvents : Monoid](f : Events => NewEvents)
  : UpdateTransformer[State, Events][IO, *] ~> UpdateTransformer[State, NewEvents][IO, *] =
    new ~>[
      UpdateTransformer[State, Events][IO, *], UpdateTransformer[State, NewEvents][IO, *]
    ]:
      override def apply[A](fa: UpdateTransformer[State, Events][IO, A]): UpdateTransformer[State, NewEvents][IO, A] =
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
  ) : [T] => UpdateTransformer[State, Events][IO, T] => IO[(Events, (State,  T))] =
    [T] => update =>
      update.run(initialState).run
  end run
end UpdateOps

trait UpdateStateOps[IO[_] : Monad, Point, Clip] extends UpdateOps[IO, UpdateState[Point, Clip]]:
  def getCornerCoordinates[Events : Monoid] : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Point] =
    getState.map(_.widgetCoordinates)
  end getCornerCoordinates

  def getClip[Events : Monoid] : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Clip] =
    getState.map(_.path)
  end getClip

  def setClip[Events : Monoid](clip : Clip) : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Unit] =
    updateState(_.withClip(clip))
  end setClip

  def markEventHandled[Events : Monoid] : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Unit] =
    updateState(_.markEventHandled)
  end markEventHandled

  def isEventHandled[Events : Monoid] : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Boolean] =
    getState.map(_.consumed)
  end isEventHandled

  def withCornerCoordinates[
    Events : Monoid,
    Value
  ](
    original : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Value],
    f : Point => Point
  ) : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Value] =
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
    original : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Value],
    f : (Clip, Point) => Clip
  ) : UpdateTransformer[UpdateState[Point, Clip], Events][IO, Value] =
    for
      coordinates <- getCornerCoordinates
      clip <- getClip
      _ <- setClip(f(clip, coordinates))
      res <- original
      _ <- setClip(clip)
    yield res
  end withClip
end UpdateStateOps
