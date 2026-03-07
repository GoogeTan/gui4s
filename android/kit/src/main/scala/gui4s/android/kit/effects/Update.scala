package gui4s.android.kit.effects

import cats.effect.IO
import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects as generic_effects
import gui4s.core.kit.effects.UpdateState

type Update[Event, A] = generic_effects.Update[IO, UpdateState[Point3d[Float], Clip], List[Event], Throwable, A]
type UpdateC[Event] = Update[Event, *]

object Update:
  given biMonadInstance : BiMonad[[A, B] =>> Update[A, B]] =
    generic_effects.Update.biMonadInstance

  def pure[Event, A](a : A) : Update[Event, A] =
    biMonadInstance().pure(a)
  end pure

  def liftK[Event] : IO ~> UpdateC[Event] =
    generic_effects.Update.liftK
  end liftK

  def getState[Event]: Update[Event, UpdateState[Point3d[Float], Clip]] =
    generic_effects.Update.getState
  end getState

  def setState[Event](state: UpdateState[Point3d[Float], Clip]): Update[Event, Unit] =
    generic_effects.Update.setState(state)
  end setState

  def updateState[Event](f: UpdateState[Point3d[Float], Clip] => UpdateState[Point3d[Float], Clip]): Update[Event, Unit] =
    generic_effects.Update.updateState(f)
  end updateState

  def emitEvents[Event](events : List[Event]): Update[Event, Unit] =
    generic_effects.Update.emitEvents(events)
  end emitEvents

  def emitEvents[Event](events : NonEmptyList[Event]): Update[Event, Unit] =
    generic_effects.Update.emitEvents(events.toList)
  end emitEvents

  def catchEvents[Event, NewEvent]: [T] => Update[Event, T] => Update[NewEvent, (T, List[Event])] =
    generic_effects.Update.catchEvents[IO, UpdateState[Point3d[Float], Clip], List[Event], List[NewEvent], Throwable]
  end catchEvents

  def mapEvents[Event, NewEvent](f : Event => NewEvent) : UpdateC[Event] ~> UpdateC[NewEvent] =
    generic_effects.Update.mapEvents(_.map(f))
  end mapEvents

  def run[Event](initialState : UpdateState[Point3d[Float], Clip])
      : [T] => Update[Event, T] => IO[Either[Throwable, (List[Event], (UpdateState[Point3d[Float], Clip],  T))]] =
    generic_effects.Update.run[IO, UpdateState[Point3d[Float], Clip], List[Event], Throwable](initialState)
  end run

  def raiseError[Event, Value](error : Throwable) : Update[Event, Value] =
    generic_effects.Update.raiseError(error)
  end raiseError

  def getCornerCoordinates[Event] : Update[Event, Point3d[Float]] =
    generic_effects.Update.getCornerCoordinates
  end getCornerCoordinates

  def getClip[Event] : Update[Event, Clip] =
    generic_effects.Update.getClip
  end getClip

  def setClip[Event](clip : Clip) : Update[Event, Unit] =
    generic_effects.Update.setClip[IO, Point3d[Float], Clip, List[Event], Throwable](clip)
  end setClip

  def markEventHandled[Event] : Update[Event, Unit] =
    generic_effects.Update.markEventHandled
  end markEventHandled

  def isEventHandled[Event] : Update[Event, Boolean] =
    generic_effects.Update.isEventHandled
  end isEventHandled

  def withCornerCoordinates[
    Event,
    Value
  ](
    original : Update[Event, Value],
    f : Point3d[Float] => Point3d[Float]
  ) : Update[Event, Value] =
    generic_effects.Update.withCornerCoordinates(original, f)
  end withCornerCoordinates

  def withClip[
    Event,
    Value
  ](
      original : Update[Event, Value],
      f : (Clip, Point3d[Float]) => Clip
  ) : Update[Event, Value] =
    generic_effects.Update.withClip(original, f)
  end withClip


  def runUpdate[Event]: [T] => Update[Event, T] => IO[Either[ExitCode, T]] =
    import Clip.given
    [T] => update =>
      run[Event](UpdateState.empty[Point3d[Float], Clip])(update).flatMap {
        case Right((_, (_, widget))) => Right(widget).pure[IO]
        case Left(error) => error.raiseError[IO, Unit].as(Left(ExitCode.Error))
      }
  end runUpdate
end Update
