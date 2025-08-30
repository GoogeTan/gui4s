package gui4s.desktop.kit
package effects

import gui4s.core.geometry.Point3d
import gui4s.core.kit.effects as generic_effects
import gui4s.core.kit.effects.UpdateState
import cats.effect.{ExitCode, IO}
import cats.*
import cats.data.EitherT
import cats.syntax.all.*
import catnip.syntax.transformer.{*, given}
import catnip.syntax.transformer.monadErrorInstanceForTransformer
import catnip.transformer.{ErrorTransformer, MonadTransformer}
import cats.kernel.Monoid

type Update[IO[_], Event, A] = generic_effects.Update[EitherT[IO, String, *], generic_effects.UpdateState[Point3d[Float], Clip], List[Event], A]
type UpdateC[IO[_], Event] = Update[IO, Event, *]

trait UpdateOps[IO[_] : Monad] extends generic_effects.UpdateStateOps[EitherT[IO, String, *], Point3d[Float], Clip]:
  given[Event] : MonadError[UpdateC[IO, Event], String] = monadErrorInstanceForTransformer

  def liftK[Event]: IO ~> UpdateC[IO, Event] =
    MonadTransformer[generic_effects.UpdateTransformer[UpdateState[Point3d[Float], Clip], List[Event]] <> ErrorTransformer[String]].liftK
  end liftK

  def raiseError[Event, T](error : String) : Update[IO, Event, T] =
    import generic_effects.given
    MonadTransformer[generic_effects.UpdateTransformer[UpdateState[Point3d[Float], Clip], List[Event]]]
      .liftK[EitherT[IO, String, *]](EitherT.left(error.pure[IO]))
  end raiseError

  def handleApplicationRequests(updateErrorAsExitCode : String => IO[ExitCode]): [T] => Update[IO, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[List[ApplicationRequest]](UpdateState.empty[Point3d[Float], Clip])(update).value.flatMap {
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
