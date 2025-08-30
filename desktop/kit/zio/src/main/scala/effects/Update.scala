package gui4s.desktop.kit.zio
package effects

import zio.*
import catnip.*
import catnip.syntax.transformer.{*, given}
import catnip.transformer.{MonadTransformer, StateTransformer}
import cats.{Monoid, ~>, MonadError}
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.kit.EventsTransformer
import gui4s.core.kit.effects.{UpdateState, UpdateStateOps, UpdateTransformer, given}
import zio.interop.catz.*

type State = UpdateState[Point3d[Float], Clip]
type Update[Event, T] = UpdateTransformer[State, List[Event]][Task, T]
type UpdateC[Event] = [Value] =>> Update[Event, Value]

@SuppressWarnings(Array("org.wartremover.warts.All"))
object Update extends UpdateStateOps[Task, Point3d[Float], Clip]:
  def liftK[Event] : Task ~> Update[Event, *] =
    MonadTransformer[UpdateTransformer[State, List[Event]]].liftK
  end liftK

  given[Event] : MonadError[UpdateC[Event], Throwable] = summon 

  def raiseError[Event, T](error : String) : Update[Event, T] =
    raiseError(new Exception(error))
  end raiseError
  
  def raiseError[Event, T](error : Throwable) : Update[Event, T] =
    liftK(
      ZIO.fail(error)
    )
  end raiseError

  def handleApplicationRequests: [T] => Update[ApplicationRequest, T] => Task[Either[ExitCode, T]] =
    [T] => update =>
      import Clip.given
      run[List[ApplicationRequest]](UpdateState.empty[Point3d[Float], Clip])(update).flatMap {
        case (events, (_, widget)) =>
          events.foldM[Task, Either[ExitCode, T]](Right(widget))((_, request) =>
            request match
              case ApplicationRequest.CloseApp(code) => ZIO.succeed(Left(code))
          )
      }
  end handleApplicationRequests
end Update

