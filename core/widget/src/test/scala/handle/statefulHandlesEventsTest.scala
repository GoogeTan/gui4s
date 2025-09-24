package gui4s.core.widget
package handle

import draw.Drawable
import merge.Mergable

import catnip.syntax.all.given
import catnip.syntax.function.andThen
import cats.data.{EitherT, NonEmptyList, Writer}
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.math.Equiv.Int

@SuppressWarnings(Array(
  "org.wartremover.warts.TripleQuestionMark",
  "org.wartremover.warts.Any"
))
final class statefulHandlesEventsTest extends AnyFlatSpec with Matchers:
  enum UpdateError:
    case StateWasUpdated
  enum PlaceError:
    case MergeWasCalled

  type Update[Event, Value] = EitherT[Writer[List[Event], *], UpdateError, Value]

  def catchEvents[Event1, Event2, Value](old : Update[Event1, Value]) : Update[Event2, (Value, List[Event1])] =
    val (efents, value) = old.value.run
    EitherT(
      Writer.value(value.map(vvalue => (vvalue, efents)))
    )
  end catchEvents

  type Place[A] = Either[PlaceError, A]

  def panicingHandlesEvent[Self, HandlableEvent, Event, Value] : HandlesEvent[Self, HandlableEvent, Update[Event, Value]] =
    (_ : Any, _ : Path, _ : Any) => EitherT.left(UpdateError.StateWasUpdated.pure)

  def panicingMerge[T] : Mergable[Place[T]] =
    (_, _, _) => Left(PlaceError.MergeWasCalled)

  def panicingDraw[T] : Drawable[T, Nothing] =
    _ => ???

  val rootPath = new Path("")

  def simpleStateful(name : String) : Stateful[Any, Int] = Stateful[Any, Int](name, 0, ())

  it should "not call merge/redraw unless there are events" in:
    statefulHandlesEvent[
      Update[Unit, *],
      Place,
      Any,
      Int,
      Unit,
      Unit
    ](
      panicingHandlesEvent[Int, NonEmptyList[Unit], Unit, Int],
      panicingDraw,
      pureEventHandler[Any, Unit, Update[Unit, Place[Any]]](value =>
        EitherT(Writer[List[Unit], Either[UpdateError, Either[PlaceError, Any]]](Nil, Right(Right(value))))
      ) andThen catchEvents,
      panicingMerge[Any],
    )(simpleStateful("name"), rootPath, ()).value.run should be ((Nil, Right(Right(simpleStateful("name")))))

  it should "not call merge/redraw unless state changed" in:
    statefulHandlesEvent[
      Update[Unit, *],
      Place,
      Any,
      Int,
      Unit,
      Unit
    ](
      pureEventHandler(_.pure[Update[Unit, *]]),
      panicingDraw,
      pureEventHandler[Any, Unit, Update[Unit, Place[Any]]](value =>
        EitherT(Writer[List[Unit], Either[UpdateError, Either[PlaceError, Any]]](Nil, Right(Right(value))))
      ) andThen catchEvents,
      panicingMerge[Any],
    )(simpleStateful("name"), rootPath, ()).value.run should be ((Nil, Right(Right(simpleStateful("name")))))
end statefulHandlesEventsTest
