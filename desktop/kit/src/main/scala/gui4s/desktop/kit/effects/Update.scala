package gui4s.desktop.kit
package effects

import catnip.BiMonad
import cats.*
import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects as generic_effects
import gui4s.core.kit.effects.UpdateState

type Update[IO[_], Event, A] = generic_effects.Update[IO, UpdateState[Point3d[Float], Clip], List[Event], Throwable, A]
type UpdateC[IO[_], Event] = Update[IO, Event, *]

object Update:
  given biMonadInstance[IO[_] : Monad] : BiMonad[[A, B] =>> Update[IO, A, B]] =
    generic_effects.Update.biMonadInstance

  def pure[IO[_] : Monad, Event, A](a : A) : Update[IO, Event, A] =
    biMonadInstance[IO]().pure(a)
  end pure

  def liftK[IO[_] : Monad, Event] : IO ~> UpdateC[IO, Event] =
    generic_effects.Update.liftK
  end liftK

  def getState[IO[_] : Monad, Event]: Update[IO, Event, UpdateState[Point3d[Float], Clip]] =
    generic_effects.Update.getState
  end getState

  def setState[IO[_] : Monad, Event](state: UpdateState[Point3d[Float], Clip]): Update[IO, Event, Unit] =
    generic_effects.Update.setState(state)
  end setState

  def updateState[IO[_] : Monad, Event](f: UpdateState[Point3d[Float], Clip] => UpdateState[Point3d[Float], Clip]): Update[IO, Event, Unit] =
    generic_effects.Update.updateState(f)
  end updateState

  def emitEvents[IO[_] : Monad, Event](events : List[Event]): Update[IO, Event, Unit] =
    generic_effects.Update.emitEvents(events)
  end emitEvents

  def emitEvents[IO[_] : Monad, Event](events : NonEmptyList[Event]): Update[IO, Event, Unit] =
    generic_effects.Update.emitEvents(events.toList)
  end emitEvents

  def catchEvents[IO[_] : Monad, Event, NewEvent]: [T] => Update[IO, Event, T] => Update[IO, NewEvent, (T, List[Event])] =
    generic_effects.Update.catchEvents[IO, UpdateState[Point3d[Float], Clip], List[Event], List[NewEvent], Throwable]
  end catchEvents

  def mapEvents[IO[_] : Monad, Event, NewEvent](f : Event => NewEvent) : UpdateC[IO, Event] ~> UpdateC[IO, NewEvent] =
    generic_effects.Update.mapEvents(_.map(f))
  end mapEvents

  def run[IO[_] : Monad, Event](initialState : UpdateState[Point3d[Float], Clip])
      : [T] => Update[IO, Event, T] => IO[Either[Throwable, (List[Event], (UpdateState[Point3d[Float], Clip],  T))]] =
    generic_effects.Update.run[IO, UpdateState[Point3d[Float], Clip], List[Event], Throwable](initialState)
  end run

  def raiseError[IO[_] : Monad, Event, Value](error : Throwable) : Update[IO, Event, Value] =
    generic_effects.Update.raiseError(error)
  end raiseError

  def getCornerCoordinates[IO[_] : Monad, Event] : Update[IO, Event, Point3d[Float]] =
    generic_effects.Update.getCornerCoordinates
  end getCornerCoordinates

  def getClip[IO[_] : Monad, Event] : Update[IO, Event, Clip] =
    generic_effects.Update.getClip
  end getClip

  def setClip[IO[_] : Monad, Event](clip : Clip) : Update[IO, Event, Unit] =
    generic_effects.Update.setClip[IO, Point3d[Float], Clip, List[Event], Throwable](clip)
  end setClip

  def markEventHandled[IO[_] : Monad, Event] : Update[IO, Event, Unit] =
    generic_effects.Update.markEventHandled
  end markEventHandled

  def isEventHandled[IO[_] : Monad, Event] : Update[IO, Event, Boolean] =
    generic_effects.Update.isEventHandled
  end isEventHandled

  def withCornerCoordinates[
    IO[_] : Monad,
    Event,
    Value
  ](
    original : Update[IO, Event, Value],
    f : Point3d[Float] => Point3d[Float]
  ) : Update[IO, Event, Value] =
    generic_effects.Update.withCornerCoordinates(original, f)
  end withCornerCoordinates

  def withClip[
    IO[_] : Monad,
    Event,
    Value
  ](
      original : Update[IO, Event, Value],
      f : (Clip, Point3d[Float]) => Clip
  ) : Update[IO, Event, Value] =
    generic_effects.Update.withClip(original, f)
  end withClip


  def handleApplicationRequests[IO[_] : MonadThrow](updateErrorAsExitCode : Throwable => IO[ExitCode]): [T] => Update[IO, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[IO, ApplicationRequest](UpdateState.empty[Point3d[Float], Clip])(update).flatMap {
        case Right((events, (_, widget))) =>
          events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
            request match
              case ApplicationRequest.CloseApp(code) => Left(code).pure[IO]
          )
        case Left(error) =>
          updateErrorAsExitCode(error).map(Left(_))
      }
  end handleApplicationRequests
end Update
