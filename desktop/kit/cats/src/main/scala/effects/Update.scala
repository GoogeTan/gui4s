package gui4s.desktop.kit.cats
package effects

import catnip.BiMonad
import catnip.syntax.transformer.{*, given}
import catnip.transformer.{ErrorTransformer, MonadTransformer, StateTransformer}
import cats.*
import cats.data.EitherT
import cats.effect.ExitCode
import cats.effect.IO
import cats.kernel.Monoid
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects.UpdateState
import gui4s.desktop.kit.common.effects as generic_effects
import gui4s.desktop.kit.common.effects.ApplicationRequest

type Update[Event, A] = generic_effects.Update[IO, Event, A]
type UpdateC[Event] = Update[Event, *]

object Update:
  given biMonadInstance : BiMonad[Update] =
    generic_effects.Update.biMonadInstance[IO]

  def liftK[Event] : IO ~> UpdateC[Event] =
    generic_effects.Update.liftK[IO, Event]
  end liftK

  def getState[Event]: Update[Event, UpdateState[Point3d[Float], Clip]] =
    generic_effects.Update.getState[IO, Event]
  end getState

  def setState[Event](state: UpdateState[Point3d[Float], Clip]): Update[Event, Unit] =
    generic_effects.Update.setState[IO, Event](state)
  end setState

  def updateState[Event](f: UpdateState[Point3d[Float], Clip] => UpdateState[Point3d[Float], Clip]): Update[Event, Unit] =
    generic_effects.Update.updateState(f)
  end updateState

  def emitEvents[Event](events : List[Event]): Update[Event, Unit] =
    generic_effects.Update.emitEvents(events)
  end emitEvents

  def catchEvents[Event, NewEvent]: [T] => Update[Event, T] => Update[NewEvent, (T, List[Event])] =
    generic_effects.Update.catchEvents[IO, Event, NewEvent]
  end catchEvents

  def mapEvents[Event, NewEvent](f : Event => NewEvent) : UpdateC[Event] ~> UpdateC[NewEvent] =
    generic_effects.Update.mapEvents(f)
  end mapEvents

  def run[Event](initialState : UpdateState[Point3d[Float], Clip])
      : [T] => Update[Event, T] => IO[Either[Throwable, (List[Event], (UpdateState[Point3d[Float], Clip],  T))]] =
    generic_effects.Update.run[IO, Event](initialState)
  end run

  def raiseError[Event, Value](error : Throwable) : Update[Event, Value] =
    generic_effects.Update.raiseError(error)
  end raiseError

  def getCornerCoordinates[Event] : Update[Event, Point3d[Float]] =
    generic_effects.Update.getCornerCoordinates[IO, Event]
  end getCornerCoordinates

  def getClip[Event] : Update[Event, Clip] =
    generic_effects.Update.getClip[IO, Event]
  end getClip

  def setClip[Event](clip : Clip) : Update[Event, Unit] =
    generic_effects.Update.setClip[IO, Event](clip)
  end setClip

  def markEventHandled[Event] : Update[Event, Unit] =
    generic_effects.Update.markEventHandled[IO, Event]
  end markEventHandled

  def isEventHandled[Event] : Update[Event, Boolean] =
    generic_effects.Update.isEventHandled[IO, Event]
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

  def handleApplicationRequests(updateErrorAsExitCode : Throwable => IO[ExitCode]): [T] => Update[ApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[ApplicationRequest](UpdateState.empty[Point3d[Float], Clip])(update).flatMap {
        case Right((events, (_, widget))) =>
          events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
            request match
              case ApplicationRequest.CloseApp(code) => Left[ExitCode, T](code).pure[IO]
          )
        case Left(error) =>
          updateErrorAsExitCode(error).map(Left[ExitCode, T])
      }
  end handleApplicationRequests
end Update
