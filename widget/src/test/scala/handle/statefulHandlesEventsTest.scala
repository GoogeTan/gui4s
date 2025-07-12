package me.katze.gui4s.widget
package handle

import catnip.syntax.all.{*, given}
import cats.data.{EitherT, NonEmptyList, OptionT, Writer}
import cats.{Id, Monad}
import cats.syntax.all.*
import me.katze.gui4s.widget.draw.Drawable
import me.katze.gui4s.widget.merge.Mergable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import me.katze.gui4s.widget.given

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
  given CatchEvents[Update] = liftEitherTCatchEvents[[Event, Value] =>> Writer[List[Event], Value], UpdateError]

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
      Update,
      Place,
      Any,
      Int,
      Unit,
      Unit,
      Unit
    ](
      panicingHandlesEvent,
      panicingDraw,
      pureEventHandler(_.pure[Place].pure[Update[Unit, *]]) andThen (_.catchEvents),
      panicingMerge[Any],
    )(simpleStateful("name"), rootPath, ()).value.run should be ((Nil, Right(simpleStateful("name"))))

  it should "not call merge/redraw unless state changed" in:
    statefulHandlesEvent[
      Update,
      Place,
      Any,
      Int,
      Unit,
      Unit,
      Unit
    ](
      pureEventHandler(_.pure[Update[Unit, *]]),
      panicingDraw,
      pureEventHandler(_.pure[Place].pure[Update[Unit, *]]) andThen (_.catchEvents),
      panicingMerge[Any],
    )(simpleStateful("name"), rootPath, ()).value.run should be ((Nil, Right(simpleStateful("name"))))
end statefulHandlesEventsTest
