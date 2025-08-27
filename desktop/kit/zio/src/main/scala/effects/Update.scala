package gui4s.desktop.kit.zio
package effects

import zio.*
import catnip.*
import catnip.syntax.transformer.{*, given}
import catnip.transformer.{MonadTransformer, StateTransformer}
import cats.{Monoid, ~>}
import gui4s.core.geometry.Point3d
import gui4s.core.kit.EventsTransformer
import gui4s.core.kit.effects.{UpdateState, UpdateStateOps}
import zio.interop.catz.*

type State = UpdateState[Point3d[Float], Clip]

type UpdateTransformer[Event] =
  StateTransformer[State] <> EventsTransformer[List[Event]]

type Update[Event, T] = UpdateTransformer[Event][ZIO[Any, String, *], T]
type UpdateC[Event] = [Value] =>> Update[Event, Value]

object Update extends UpdateStateOps[IO[String, *], Point3d[Float], Clip]:
  def liftK[Event] : IO[String, *] ~> Update[Event, *] =
    MonadTransformer[UpdateTransformer[Event]].liftK
  end liftK

  def raiseError[Event, T](error : String) : Update[Event, T] =
    liftK(
      ZIO.fail(error)
    )
  end raiseError

  def handleApplicationRequests(updateErrorAsExitCode: String => UIO[ExitCode]): [T] => Update[ApplicationRequest, T] => UIO[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[List[ApplicationRequest]](UpdateState.empty[Point3d[Float], Clip])(update).either.flatMap {
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

