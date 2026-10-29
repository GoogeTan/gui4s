package gui4s.desktop.kit
package effects

import scala.reflect.Typeable

import catnip.BiMonad
import cats.*
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*

import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects.UpdateContext
import gui4s.core.widget.Path

import gui4s.desktop.kit.effects.Clip.given
import gui4s.desktop.kit.effects.DownEvent.UserEvent

type UpdateC[Event] = Update[Event, *]

opaque type Update[Event, A] <: AnyRef =
  (
    UpdateContext[Point3d[Float], Clip],
    List[DownEvent]
  ) => IO[(List[Event], List[DownEvent], A)]

object Update:
  inline def apply[Event, A](inline runEffect: (UpdateContext[Point3d[Float], Clip], List[DownEvent]) => IO[(List[Event], List[DownEvent], A)]): Update[Event, A] =
    runEffect

  private type Ctx = UpdateContext[Point3d[Float], Clip]
  private type St  = List[DownEvent]

  extension[Event, A](self : Update[Event, A])
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Equals"))
    def map[B](f: A => B): Update[Event, B] =
      Update((ctx, state) =>
        self(ctx, state).map { case k @ (events, nextState, a) =>
          val b = f(a)
          if b.asInstanceOf[AnyRef] eq a.asInstanceOf[AnyRef] then//prevents extra allocations
            k.asInstanceOf[(List[Event], List[DownEvent], B)]
          else
            (events, nextState, b)
        }
      )

    def flatMap[B](f: A => Update[Event, B]): Update[Event, B] =
      Update((ctx, state) =>
        self(ctx, state).flatMap { (events1, state1, a) =>
          if events1.isEmpty then
            f(a)(ctx, state1)
          else
            f(a)(ctx, state1).map { (events2, state2, b) =>
              (events1 ++ events2, state2, b)
            }
        }
      )

  given monadInstance[Event]: Monad[UpdateC[Event]] = new Monad[UpdateC[Event]]:
    override def pure[A](x: A): Update[Event, A] =
      Update((_, state) => IO.pure((Nil, state, x)))

    override def flatMap[A, B](fa: Update[Event, A])(f: A => Update[Event, B]): Update[Event, B] =
      fa.flatMap(f)

    override def map[A, B](fa: Update[Event, A])(f: A => B): Update[Event, B] =
      fa.map(f)

    override def tailRecM[A, B](a: A)(f: A => Update[Event, Either[A, B]]): Update[Event, B] =
      Update((ctx, initialState) =>
        Monad[IO].tailRecM((List.empty[Event], initialState, a)) { case (accEvents, curState, curA) =>
          f(curA)(ctx, curState).map {
            case (newEvents, nextState, Left(nextA)) =>
              Left((accEvents ++ newEvents, nextState, nextA))
            case (newEvents, nextState, Right(b)) =>
              Right((accEvents ++ newEvents, nextState, b))
          }
        }
      )
  end monadInstance

  given biMonadInstance: BiMonad[[A, B] =>> Update[A, B]] =
    [T] => () => monadInstance[T]

  def pure[Event, A](a: A): Update[Event, A] =
    monadInstance[Event].pure(a)
  end pure

  val unitInstance: Update[Nothing, Unit] =
    pure(())
  end unitInstance

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unit[Event]: Update[Event, Unit] =
    unitInstance.asInstanceOf[Update[Event, Unit]]
  end unit

  def liftK[Event]: IO ~> UpdateC[Event] =
    new (IO ~> UpdateC[Event]):
      def apply[A](fa: IO[A]): Update[Event, A] =
        Update((_, state) => fa.map(a => (Nil, state, a)))
  end liftK

  def getContext[Event]: Update[Event, Ctx] =
    Update((ctx, state) => IO.pure((Nil, state, ctx)))
  end getContext

  def getState[Event]: Update[Event, St] =
    Update((_, state) => IO.pure((Nil, state, state)))
  end getState

  def setState[Event](state: St): Update[Event, Unit] =
    Update((_, _) => IO.pure((Nil, state, ())))
  end setState

  def updateState[Event](f: St => St): Update[Event, Unit] =
    Update((_, state) => IO.pure((Nil, f(state), ())))
  end updateState

  def emitEnvironmentalEvents[Event](events: List[DownEvent]): Update[Event, Unit] =
    updateState(_ |+| events)
  end emitEnvironmentalEvents

  def emitEvents[Event](events: List[Event]): Update[Event, Unit] =
    Update((_, state) => IO.pure((events, state, ())))
  end emitEvents

  def emitEvent[Event](events: Event): Update[Event, Unit] =
    emitEvents(List(events))
  end emitEvent

  def emitEvents[Event](events: NonEmptyList[Event]): Update[Event, Unit] =
    emitEvents(events.toList)
  end emitEvents

  def catchEvents[Event, NewEvent]: [T] => Update[Event, T] => Update[NewEvent, (List[Event], T)] =
    [T] => update => (context, events) =>
      update(context, events).map((events, nextState, res) => (Nil, nextState, (events, res)))
  end catchEvents

  def mapEvents[Event, NewEvent](f: Event => NewEvent): UpdateC[Event] ~> UpdateC[NewEvent] =
    FunctionK.lift(
      [T] => update =>
        Update((ctx, state) =>
          update(ctx, state).map { (events, nextState, res) =>
            (events.map(f), nextState, res)
          }
        )
    )
  end mapEvents

  def run[Event](initialContext: Ctx, initialState: St)
  : [T] => Update[Event, T] => IO[(List[Event], St, T)] =
    [T] => update =>
      update(initialContext, initialState)
  end run

  def raiseError[Event, Value](error: => Throwable): Update[Event, Value] =
    liftK(IO.raiseError(error))
  end raiseError

  def raiseError[Event, Value](error: Path => Throwable): Update[Event, Value] =
    Update((ctx, _) => IO.raiseError(error(ctx.path)))
  end raiseError

  def getCornerCoordinates[Event]: Update[Event, Point3d[Float]] =
    Update((ctx, state) => IO.pure((Nil, state, ctx.widgetCornerCoordinates)))
  end getCornerCoordinates

  def getClip[Event]: Update[Event, Clip] =
    Update((ctx, state) => IO.pure((Nil, state, ctx.clip)))
  end getClip

  def currentPath[Event]: Update[Event, Path] =
    Update((ctx, state) => IO.pure((Nil, state, ctx.path)))
  end currentPath

  def addNameToPath[Event](name: String): Update[Event, *] ~> Update[Event, *] =
    new (Update[Event, *] ~> Update[Event, *]):
      def apply[A](fa: Update[Event, A]): Update[Event, A] =
        Update((ctx, state) => fa(ctx.addNameToThePath(name), state))
  end addNameToPath

  def withState[Event, Value](
    original: Update[Event, Value],
    f: St => St
  ): Update[Event, Value] =
    for
      oldState <- getState
      _ <- setState(f(oldState))
      res <- original
      _ <- setState(oldState)
    yield res
  end withState

  //TODO А почему тут вообще 3д точка?
  def withCornerCoordinates[Event, Value](
    original: Update[Event, Value],
    f: Point3d[Float] => Point3d[Float]
  ): Update[Event, Value] =
    Update((ctx, state) =>
      original(ctx.withCoordinates(f(ctx.widgetCornerCoordinates)), state)
    )
  end withCornerCoordinates

  def withClip[Event, Value](
    original: Update[Event, Value],
    f: (Clip, Point3d[Float]) => Clip
  ): Update[Event, Value] =
    Update((ctx, state) =>
      original(ctx.withClip(f(ctx.clip, ctx.widgetCornerCoordinates)), state)
    )
  end withClip

  def updateEnvironmentalEventsM[Event, Result](
    f: List[DownEvent] => Update[Event, (Result, List[DownEvent])]
  ): Update[Event, Result] =
    for
      state <- getState
      (res, newDownEvents) <- f(state)
      _ <- setState(newDownEvents)
    yield res
  end updateEnvironmentalEventsM

  def updateEnvironmentalEventsM_[Event](
    f: List[DownEvent] => Update[Event, List[DownEvent]]
  ): Update[Event, Unit] =
    for
      state <- getState
      newDownEvents <- f(state)
      _ <- setState(newDownEvents)
    yield ()
  end updateEnvironmentalEventsM_

  def handleEnvironmentalEvents[Event, Result](
    consumed: => (DownEvent => Update[Event, (Result, Boolean)])
  ): Update[Event, List[Result]] =
    updateEnvironmentalEventsM(events =>
      if events.isEmpty then
        (Nil, Nil).pure[UpdateC[Event]]
      else
        val c = consumed
        events.traverse(c).map((smth: List[(Result, Boolean)]) =>
          val (results, isConsumed) = smth.unzip
          (results, events.zip(isConsumed).filter(!_._2).map(_._1))
        )
    )
  end handleEnvironmentalEvents

  def handleEnvironmentalEvents_[Event](
    consumed: => (DownEvent => Update[Event, Boolean])
  ): Update[Event, Unit] =
    updateEnvironmentalEventsM_(events =>
      if events.isEmpty then
        Nil.pure[UpdateC[Event]]
      else
        val c = consumed
        events.filterA[Update[Event, *]](event => c(event).map(!_))
    )
  end handleEnvironmentalEvents_

  def handleUserEvents_[Event, T: Typeable](f: T => Update[Event, Boolean]): Update[Event, Unit] =
    handleEnvironmentalEvents_ {
      case UserEvent(value: T) =>
        f(value)
      case _ =>
        false.pure[UpdateC[Event]]
    }
  end handleUserEvents_

  def handleUserEvents[Event, T: Typeable, Result](
    f: T => Update[Event, (Result, Boolean)]
  ): Update[Event, List[Result]] =
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

  def handleExternalEvents[Event, Result](
    f: Any => Update[Event, (Result, Boolean)]
  ): Update[Event, List[Result]] =
    currentPath.flatMap(path =>
      handleEnvironmentalEvents(event =>
        DownEvent.catchExternalEvent(path, event).fold(
          (Nil, false).pure[UpdateC[Event]]
        )((value: Any) => f(value).map((result, isConsumed) => (List(result), isConsumed)))
      ).map(_.flatten)
    )
  end handleExternalEvents

  //saving memory
  private val emptyContext = UpdateContext.empty[Point3d[Float], Clip]

  def runUpdate[Event]: [T] => (Update[Event, T], List[DownEvent]) => IO[Either[ExitCode, T]] =
    [T] => (update, events) =>
      run[Event](
        emptyContext,
        events
      )(update).map {
        case (_, _, widget) => Right(widget)
      }
  end runUpdate

  def println[Event](text: String): Update[Event, Unit] =
    liftK(IO.println(text))
  end println
end Update