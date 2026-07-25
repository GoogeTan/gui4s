package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import cats.*
import cats.syntax.all.*
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.MergeStates
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.deferred
import scala.reflect.Typeable

trait StatefulTests extends AnyFlatSpec with Matchers with Inside:
  type IO[_]
  type Widget[_]
  type Place[_] : Monad
  type Update[_, _]
  type DownEvent : Eq

  given updateIsMonadForallEvents[Event]: Monad[Update[Event, *]] = deferred

  def statefulWidget[State : Typeable, OwnEvent, ChildEvent](
    name : String,
    initialState : Place[State],
    stateAsFree : State => Place[Option[State]],
    handleEvent : HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render : State => Widget[ChildEvent],
    mergeStates : MergeStates[Place, State]
  ) : Widget[OwnEvent]

  def simpleStatefulWidget[State: {Eq, Typeable}, OwnEvent, ChildEvent](
    name: String,
    initialState: State,
    handleEvent: HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render: State => Widget[ChildEvent],
  ): Widget[OwnEvent]

  def drawOnly[Event](rendersTo : String) : Widget[Event]

  /**
   * On each update throws an event unconditionally
   */
  def eventThrower[Event](i: Event, value: Widget[Event]) : Widget[Event]

  def throwEvents[Event](events : List[Event]) : Update[Event, Unit]

  def update[Event](widget: Widget[Event], downEvents : List[DownEvent]) : IO[(List[Event], Widget[Event])]

  def insideIO[T](value : IO[T])(f : PartialFunction[T, Unit]) : Unit

  extension[Event](widget : Widget[Event])
    infix def shouldDrawInto(value : String) : Unit
  end extension

  "Stateful widget" should "render initial state correctly" in:
    val widgetToTest = simpleStatefulWidget(
      name = "myStatefulWidget",
      initialState = 10,
      handleEvent =
        (state, event) =>
          if event.isEmpty then
            None.pure
          else
            Some(state + event.length).pure[Update[Nothing, *]],
      render = state =>
        drawOnly(s"State: $state"),
    )

    widgetToTest shouldDrawInto "State: 10"

  it should "handle events and update its state" in:
    val eventToThrow = 7
    val widgetToTest = simpleStatefulWidget[Int, Nothing, Int](
      name = "myStatefulWidget",
      initialState = 10,
      handleEvent =
        (state, events) =>
          //Only one event, which was thrown by eventThrower, is expected here.
          inside(events):
            case `eventToThrow` :: Nil =>
              Some(state + eventToThrow).pure[Update[Nothing, *]],
      render = state =>
        eventThrower(
          eventToThrow,
          drawOnly(s"State: $state"),
        ),
    )

    insideIO(
      update(widgetToTest, Nil)
    ):
      (events, widget) =>
        events should be(Nil)
        widget shouldDrawInto "State: 17"

  it should "handle nested events correctly" in:
    val eventToThrow = 7
    val widgetToTest = simpleStatefulWidget[Int, Nothing, Int](
      name = "statefulWidget",
      initialState = 10,
      handleEvent =
        (state, events) =>
          inside(events):
            case 17 :: Nil =>
              Some(state + 17).pure[Update[Nothing, *]],
      render = outerState =>
        simpleStatefulWidget[Int, Int, Int](
          name = "nestedStatefulWidget",
          initialState = 10,
          handleEvent =
            (state, events) =>
              inside(events):
                case `eventToThrow` :: Nil =>
                  throwEvents(
                    List(
                      state + eventToThrow
                    )
                  ).as(Some(state + eventToThrow)),
          render = innerState =>
            eventThrower(
              eventToThrow,
              drawOnly(s"States: $outerState $innerState"),
            ),
        )
    )

    insideIO(
      update(widgetToTest, Nil)
    ):
      (events, widget) =>
        events should be(Nil)
        widget shouldDrawInto "States: 27 17"
end StatefulTests
