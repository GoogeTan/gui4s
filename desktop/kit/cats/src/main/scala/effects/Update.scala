package gui4s.desktop.kit.cats
package effects

import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects as generic_effects
import gui4s.core.kit.effects.UpdateState
import cats.effect.{ExitCode, IO}
import cats.*
import cats.syntax.all.*

type Update[Event, A] = generic_effects.Update[IO, String, generic_effects.UpdateState[Point3d[Float], Clip], List[Event], A]
type UpdateC[Event] = Update[Event, *]

object Update extends generic_effects.UpdateStateOps[IO, String, Point3d[Float], Clip]:
  def handleApplicationRequests(updateErrorAsExitCode : String => IO[ExitCode]): [T] => Update[ApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[List[ApplicationRequest]](UpdateState.empty[Point3d[Float], Clip])(update).flatMap {
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
