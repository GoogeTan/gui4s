package gui4s.desktop.kit
package effects

import scala.reflect.Typeable

import catnip.BiMonad
import catnip.syntax.transformer as stateT2Instance
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import cats.*
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*

import gui4s.core.geometry.Point3d
import gui4s.core.kit.EventsTransformer
import gui4s.core.kit.effects.UpdateContext
import gui4s.core.kit.effects.UpdateState
import gui4s.core.widget.Path

import gui4s.desktop.kit.effects.Clip.given
import gui4s.desktop.kit.effects.DownEvent.UserEvent

type UpdateTransformer[Event] =
  ReaderTransformer[UpdateContext[Point3d[Float], Clip]]
    <> StateTransformer[UpdateState[List[DownEvent]]]
    <> EventsTransformer[List[Event]]

type Update[Event, A] = UpdateTransformer[Event][IO, A]
type UpdateC[Event] = Update[Event, *]

object Update:
  given mt[Event]: MonadTransformer[UpdateTransformer[Event]] =
    import catnip.syntax.transformer.{given, *}
    composedMonadTransformerInstance[
      ReaderTransformer[UpdateContext[Point3d[Float], Clip]],
      [IO[_], T] =>> (StateTransformer[UpdateState[List[DownEvent]]] <> EventsTransformer[List[Event]])[IO, T]
    ]

  given biMonadInstance: BiMonad[[A, B] =>> Update[A, B]] =
    [T] => () => mt.monadInstance[IO]

  def pure[Event, A](a: A): Update[Event, A] =
    biMonadInstance().pure(a)
  end pure

  def unit[Event]: Update[Event, Unit] =
    pure(())
  end unit

  def liftK[Event]: IO ~> UpdateC[Event] =
    mt.liftK
  end liftK
  
  def getContext[Event]: Update[Event, UpdateContext[Point3d[Float], Clip]] =
    ReaderTransformer.ask_
  end getContext
  
  def getState[Event]: Update[Event, UpdateState[List[DownEvent]]] =
    StateTransformer.get
  end getState

  def setState[Event](state: UpdateState[List[DownEvent]]): Update[Event, Unit] =
    StateTransformer.set(state)
  end setState

  def updateState[Event](f: UpdateState[List[DownEvent]] => UpdateState[List[DownEvent]]): Update[Event, Unit] =
    StateTransformer.modify(f)
  end updateState

  def emitEnvironmentalEvents[Event](events: List[DownEvent]): Update[Event, Unit] =
    updateState(_.emitEnvironmentalEvents(events))
  end emitEnvironmentalEvents

  def emitEvents[Event](events : List[Event]): Update[Event, Unit] =
    EventsTransformer.raiseEvents(events)
  end emitEvents

  def emitEvent[Event](events: Event): Update[Event, Unit] =
    emitEvents(List(events))
  end emitEvent

  def emitEvents[Event](events : NonEmptyList[Event]): Update[Event, Unit] =
    EventsTransformer.raiseEvents(events.toList)
  end emitEvents

  def catchEvents[Event, NewEvent]: [T] => Update[Event, T] => Update[NewEvent, (T, List[Event])] =
    [T] => update =>
      for
        context <- getContext
        stateBefore <- getState[NewEvent]
        resWithEventsAndState <- liftK[NewEvent](Update.run[Event](context, stateBefore)(update))
        (events, (stateAfter, res)) = resWithEventsAndState
        _ <- setState(stateAfter)
      yield (res, events)
  end catchEvents

  def mapEvents[Event, NewEvent](f: Event => NewEvent): UpdateC[Event] ~> UpdateC[NewEvent] =
    FunctionK.lift(
      [T] => update =>
        for
          tmp <- catchEvents[Event, NewEvent](update)
          (res, events) = tmp
          _ <- emitEvents(events.map(f))
        yield res
    )
  end mapEvents

  def run[Event](initialContext: UpdateContext[Point3d[Float], Clip], initialState: UpdateState[List[DownEvent]])
      : [T] => Update[Event, T] => IO[(List[Event], (UpdateState[List[DownEvent]],  T))] =
    [T] => update =>
      update.run(initialContext).run(initialState).run
  end run

  def raiseError[Event, Value](error : => Throwable) : Update[Event, Value] =
    liftK(IO.raiseError(error))
  end raiseError

  def raiseError[Event, Value](error: Path => Throwable): Update[Event, Value] =
    currentPath.flatMap(path => liftK(IO.raiseError(error(path))))
  end raiseError

  def getCornerCoordinates[Event] : Update[Event, Point3d[Float]] =
    ReaderTransformer.ask_.map(_.widgetCornerCoordinates)
  end getCornerCoordinates

  def getClip[Event] : Update[Event, Clip] =
    ReaderTransformer.ask_.map(_.clip)
  end getClip
  
  def currentPath[Event] : Update[Event, Path] =
    ReaderTransformer.ask_.map(_.path)
  end currentPath
  
  def addNameToPath[Event](name : String) : Update[Event, *] ~> Update[Event, *] =
    ReaderTransformer.withValueK_(_.addNameToThePath(name))
  end addNameToPath

  def withState[Event, Value](
                               original: Update[Event, Value],
                               f: UpdateState[List[DownEvent]] => UpdateState[List[DownEvent]]
                             ): Update[Event, Value] =
    for
      oldState <- getState
      _ <- setState(f(oldState))
      res <- original
      _ <- setState(oldState)
    yield res
  end withState

  //TODO А почему тут вообще 3д точка?
  def withCornerCoordinates[
    Event,
    Value
  ](
    original: Update[Event, Value],
    f: Point3d[Float] => Point3d[Float]
  ): Update[Event, Value] =
    ReaderTransformer.withValue_(original, ctx => ctx.withCoordinates(f(ctx.widgetCornerCoordinates)))
  end withCornerCoordinates

  def withClip[
    Event,
    Value
  ](
    original: Update[Event, Value],
    f: (Clip, Point3d[Float]) => Clip
  ): Update[Event, Value] =
    ReaderTransformer.withValue_(original, ctx => ctx.withClip(f(ctx.clip, ctx.widgetCornerCoordinates)))
  end withClip
    
  def updateEnvironmentalEventsM[Event, Result](f : List[DownEvent] => Update[Event, (Result, List[DownEvent])]) : Update[Event, Result] =
    for
      state <- getState
      (res, newDownEvents) <- f(state.environmentalEvents)
      _ <- setState(state.withEnvironmentalEvents(newDownEvents))
    yield res
  end updateEnvironmentalEventsM

  def updateEnvironmentalEventsM_[Event](f: List[DownEvent] => Update[Event, List[DownEvent]]): Update[Event, Unit] =
    for
      state <- getState
      newDownEvents <- f(state.environmentalEvents)
      _ <- setState(state.withEnvironmentalEvents(newDownEvents))
    yield ()
  end updateEnvironmentalEventsM_

  def handleEnvironmentalEvents[Event, Result](consumed : DownEvent => Update[Event, (Result, Boolean)]) : Update[Event, List[Result]] =
    updateEnvironmentalEventsM(events =>
      events.traverse(consumed).map((smth : List[(Result, Boolean)]) =>
        val (results, isConsumed) = smth.unzip
        (results, events.zip(isConsumed).filter(!_._2).map(_._1))
      )
    )
  end handleEnvironmentalEvents

  def handleEnvironmentalEvents_[Event](consumed: DownEvent => Update[Event, Boolean]): Update[Event, Unit] =
    updateEnvironmentalEventsM_(events =>
      events.filterA(event => consumed(event).map(!_))
    )
  end handleEnvironmentalEvents_

  def handleUserEvents_[Event, T : Typeable](f: T => Update[Event, Boolean]): Update[Event, Unit] =
    handleEnvironmentalEvents_ {
      case UserEvent(value: T) =>
        f(value)
      case _ =>
        false.pure[UpdateC[Event]]
    }
  end handleUserEvents_

  def handleUserEvents[Event, T: Typeable, Result](f: T => Update[Event, (Result, Boolean)]): Update[Event, List[Result]] =
    handleEnvironmentalEvents {
      case UserEvent(value: T) =>
        f(value).map((result, isConsumed) => (List(result), isConsumed))
      case _ =>
        (Nil, false).pure[UpdateC[Event]]
    }.map(_.flatten)
  end handleUserEvents

  def handleExternalEvents_[Event](f: Any => Update[Event, Boolean]): Update[Event, Unit] =
    currentPath.flatMap(path =>
      handleEnvironmentalEvents_(event =>
        DownEvent.catchExternalEvent(path, event).fold(
          false.pure[UpdateC[Event]]
        )(f)
      )
    )
  end handleExternalEvents_

  def handleExternalEvents[Event, Result](f: Any => Update[Event, (Result, Boolean)]): Update[Event, List[Result]] =
    currentPath.flatMap(path =>
      handleEnvironmentalEvents(event =>
        DownEvent.catchExternalEvent(path, event).fold(
          (Nil, false).pure[UpdateC[Event]]
        )((value : Any) => f(value).map((result, isConsumed) => (List(result), isConsumed)))
      ).map(_.flatten)
    )
  end handleExternalEvents

  def runUpdate[Event]: [T] => (Update[Event, T], List[DownEvent]) => IO[Either[ExitCode, T]] =
    [T] => (update, events) =>
      run[Event](
        UpdateContext.empty[Point3d[Float], Clip],
        UpdateState.empty[List[DownEvent]].withEnvironmentalEvents(events)
      )(update)
        .map {
        case (_, (state, widget)) => Right(widget)
      }
  end runUpdate

  def println[Event](text : String) : Update[Event, Unit] =
    liftK(IO.println(text))
  end println
end Update
