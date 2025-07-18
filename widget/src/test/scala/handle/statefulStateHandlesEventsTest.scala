package me.katze.gui4s.widget
package handle

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.functor.*
import me.katze.gui4s.widget.handle.statefulStateHandlesEvents
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import me.katze.gui4s.widget.{Path, StatefulBehaviour}

final class statefulStateHandlesEventsTest extends AnyFlatSpec with Matchers:
  val path = new Path("parent")

  "StatefulStateHandlesEvents" should "update state when handling events" in {
    type TestState = Int
    type TestHandler = (TestState, Path, String) => Id[TestState]

    val handler: TestHandler = (state, _, events) => state + events.length

    val initial = StatefulBehaviour(
      name = "test",
      state = StatefulState(
        initialState = 0,
        currentState = 0,
      ),
      draw = (),
      handleEvents = handler,
      destructor = ()
    )

    val event = "event1"
    val result = statefulStateHandlesEvents[Id, TestState, Unit, String, Unit](initial, path, event)
    result.state should be(StatefulState(0, 6))
  }

  it should "preserve other fields when updating state" in {
    case class TestState(value: Int)
    type TestHandler = (TestState, Path, String) => Id[TestState]

    val handler: TestHandler = (state, _, events) =>
      TestState(state.value + events.length)

    val initial = StatefulBehaviour(
      "test",
      StatefulState(TestState(0), TestState(0)),
      "draw",
      handler,
      "destructor"
    )

    val events = "event1"
    statefulStateHandlesEvents[
      Id,
      TestState,
      String,
      String,
      String
    ](initial, path, events) should be(initial.copy(state = StatefulState(TestState(0), TestState(6))))
  }

