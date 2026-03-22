package gui4s.android.kit.effects

import catnip.syntax.all.{*, given}
import catnip.syntax.transformer.{*, given}
import catnip.transformer.{MonadTransformer, StateTransformer}
import cats.effect.IO
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.kit.{EventsTransformer, effects as generic_effects}
import gui4s.core.kit.effects.UpdateState
import Clip.given
import gui4s.core.widget.Path

import scala.reflect.Typeable

type UpdateTransformer[Event] =
  (StateTransformer[UpdateState[List[DownEvent], Point3d[Float], Clip]] <> EventsTransformer[List[Event]])

type Update[Event, A] = UpdateTransformer[Event][IO, A]
type UpdateC[Event] = Update[Event, *]

object Update:
  given mt[Event]: MonadTransformer[UpdateTransformer[Event]] =
    composedMonadTransformerInstance

  given biMonadInstance : BiMonad[[A, B] =>> Update[A, B]] =
    [T] => () => mt.monadInstance[IO]

  def pure[Event, A](a : A) : Update[Event, A] =
    biMonadInstance().pure(a)
  end pure

  def liftK[Event] : IO ~> UpdateC[Event] =
    mt.liftK
  end liftK

  def getState[Event]: Update[Event, UpdateState[List[DownEvent], Point3d[Float], Clip]] =
    StateTransformer.get_
  end getState

  def setState[Event](state: UpdateState[List[DownEvent], Point3d[Float], Clip]): Update[Event, Unit] =
    StateTransformer.set_(state)
  end setState

  def updateState[Event](f: UpdateState[List[DownEvent], Point3d[Float], Clip] => UpdateState[List[DownEvent], Point3d[Float], Clip]): Update[Event, Unit] =
    StateTransformer.modify_(f)
  end updateState

  def emitEvents[Event](events : List[Event]): Update[Event, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def emitEvents[Event](events : NonEmptyList[Event]): Update[Event, Unit] =
    EventsTransformer.raiseEvents(events.toList)
  end emitEvents

  def catchEvents[Event, NewEvent]: [T] => Update[Event, T] => Update[NewEvent, (T, List[Event])] =
    [T] => update =>
      EventsTransformer.catchEvents(update)
  end catchEvents

  def mapEvents[Event, NewEvent](f : Event => NewEvent) : UpdateC[Event] ~> UpdateC[NewEvent] =
    FunctionK.lift(
      [T] => update =>
        EventsTransformer.mapEvents(update, _.map(f))
    )
  end mapEvents

  def run[Event](initialState : UpdateState[List[DownEvent], Point3d[Float], Clip])
  : [T] => Update[Event, T] => IO[(List[Event], (UpdateState[List[DownEvent], Point3d[Float], Clip],  T))] =
    [T] => update =>
      update.run(initialState).run
  end run

  def raiseError[Event, Value](error : Throwable) : Update[Event, Value] =
    liftK(IO.raiseError(error))
  end raiseError

  def getCornerCoordinates[Event] : Update[Event, Point3d[Float]] =
    getState.map(_.widgetCoordinates)
  end getCornerCoordinates

  def getClip[Event] : Update[Event, Clip] =
    getState.map(_.clip)
  end getClip

  def setClip[Event](clip : Clip) : Update[Event, Unit] =
    updateState(_.withClip(clip))
  end setClip

  def withState[Event, Value](
                               original : Update[Event, Value],
                               f : UpdateState[List[DownEvent], Point3d[Float], Clip] => UpdateState[List[DownEvent], Point3d[Float], Clip]
                             ) : Update[Event, Value] =
    for
      oldState <- getState
      _ <- setState(f(oldState))
      res <- original
      newState <- getState
      _ <- setState(newState.withCoordinates(oldState.widgetCoordinates).withClip(oldState.clip))
    yield res
  end withState

  //TODO А почему тут вообще 3д точка?
  def withCornerCoordinates[
    Event,
    Value
  ](
     original : Update[Event, Value],
     f : Point3d[Float] => Point3d[Float]
   ) : Update[Event, Value] =
    withState(
      original,
      state =>
        state.withCoordinates(f(state.widgetCoordinates))
    )
  end withCornerCoordinates

  def withClip[
    Event,
    Value
  ](
     original : Update[Event, Value],
     f : (Clip, Point3d[Float]) => Clip
   ) : Update[Event, Value] =
    withState(
      original,
      state =>
        state.withClip(f(state.clip, state.widgetCoordinates))
    )
  end withClip

  def handleEnvironmentalEvents[Event](consumed : DownEvent => Update[Event, Boolean]) : Update[Event, Unit] =
    for
      state <- getState
      newDownEvents <- state.environmentalEvents.filterA(event => consumed(event).map(!_))
      _ <- setState(state.withEnvironmentalEvents(newDownEvents))
    yield ()
  end handleEnvironmentalEvents

  def handleExternalEvents[Event](consumed : Any => Update[Event, Boolean], path : Path) : Update[Event, Unit] =
    handleEnvironmentalEvents(
      event =>
        DownEvent.catchExternalEvent(path, event).fold(
          false.pure[UpdateC[Event]]
        )(consumed)
    )
  end handleExternalEvents


  def handleUserEvents[Event, T : Typeable](f: T => Update[Event, Boolean]): Update[Event, Unit] =
    handleEnvironmentalEvents:
      case DownEvent.UserEvent(value : T) =>
        f(value)
      case _ =>
        false.pure[UpdateC[Event]]
  end handleUserEvents

  def runUpdate[Event]: [T] => (Update[Event, T], List[DownEvent]) => IO[Either[ExitCode, T]] =
    [T] => (update, events) =>
      run[Event](UpdateState.empty[
        List[DownEvent], Point3d[Float], Clip
      ].withEnvironmentalEvents(events))(update)
        .map {
          case (_, (_, widget)) => Right(widget)
        }
  end runUpdate
end Update
