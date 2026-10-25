package gui4s.desktop.widget.library

import scala.reflect.Typeable

import catnip.CodensityT
import catnip.syntax.additional.*
import cats.*
import cats.arrow.FunctionK
import cats.data.OptionT
import cats.data.WriterT
import cats.syntax.all.*
import cats.~>

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.MergeStates

class SimpleStatefulTest extends AnyFlatSpec with Matchers with Inside with WidgetTestingSpec:
  override type Update[Event, T] = WriterT[IO, List[Event], T]
  override given updateMonad[Event]: Monad[Update[Event, *]] = summon[Monad[Update[Event, *]]]

  override type Place[T] = OptionT[IO, T]
  override type RecompositionReaction = Unit
  override type Draw = String
  // Use eval to mimic real IO laziness
  override type IO[T] = Eval[T]
  override given ioMonad : Monad[IO] = summon[Monad[Eval]]

  override def runUpdate[T, Event](update : Update[Event, T]) : IO[(List[Event], T)] =
    update.run
  end runUpdate

  override def runRecomposition : RecompositionReaction => IO[Unit] =
    _ => Eval.Unit

  def widgetsAreMergeable[Event] =
    widgetsCanUpdateStateFromTheOldOnes[PlacedTestWidget[Event], Place, Id, RecompositionReaction](
      widgetMergesWithOldState,
      widgetHasInnerStates
    )

  def liftUpdate[OwnEvent, ChildEvent] : [T] => (fa: Update[ChildEvent, T]) => Update[OwnEvent, (List[ChildEvent], T)] =
    [T] => (fa: Update[ChildEvent, T]) =>
      WriterT.liftF(fa.run)
  end liftUpdate

  def throwEvent[Event](events : List[Event]) : Update[Event, Unit] =
    WriterT.tell(events)
  end throwEvent

  val noRecomposition: RecompositionReaction = ()

  def drawOnly[Event](draw : Draw) : TestWidget[Event] =
    constanctSizeDrawOnlyWidget[Update[Event, *], Place, Id, Draw, RecompositionReaction](draw, noRecomposition)
  end drawOnly

  def statefulWidget[State, OwnEvent, ChildEvent](
    name : String,
    initialState : Place[State],
    stateAsFree : State => Place[Option[State]],
    handleEvent : HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render : State => TestWidget[ChildEvent],
    mergeStates : MergeStates[Place, State]
  ) : TestWidget[OwnEvent] =
    gui4s.desktop.widget.library.stateful[
      Update[OwnEvent, *],
      Update[ChildEvent, *],
      Place,
      Draw,
      RecompositionReaction,
      State,
      List[ChildEvent]
    ](
      widgetsAreMergeable = widgetsAreMergeable,
      liftUpdate = liftUpdate[OwnEvent, ChildEvent],
      addNameToUpdatePath = _ => FunctionK.id,
      addNameToPlacePath = _ => FunctionK.id,
      noRecomposition = noRecomposition
    )(
      name = name,
      initialState = CodensityT.liftF(initialState),
      stateAsFree = st => CodensityT.liftF(stateAsFree(st)),
      handleEvent = handleEvent,
      render = render,
      mergeStates = mergeStates
    )
  end statefulWidget

  def simpleStatefulWidget[State : {Eq, Typeable}, OwnEvent, ChildEvent](
    name : String,
    initialState : State,
    handleEvent : HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render : State => TestWidget[ChildEvent],
  ) : TestWidget[OwnEvent] =
    statefulWidget(name, initialState.pure, _ => None.pure, handleEvent, render, MergeStates.default([T] => _ => OptionT.none))
  end simpleStatefulWidget

  def eventThrower[Event](eventToThrow : Event, body : TestWidget[Event]) : TestWidget[Event] =
    simpleStatefulWidget[Unit, Event, Event](
      name = "thrower",
      initialState = (),
      handleEvent = (_, events) => throwEvent(eventToThrow :: events).as(None),
      render = _ => body,
    )
  end eventThrower

  override val runPlace : Place ~> IO = FunctionK.lift(
    [T] => (place : Place[T]) =>
      Eval.later(
        inside(place.value.value):
          case Some(v) => v
      )
  )

  "Stateful widget" should "render initial state correctly" in {
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

    runPlace(widgetToTest).value.draw should be("State: 10")
  }

  it should "handle events and update its state" in {
    val eventToThrow = 7
    val widgetToTest = simpleStatefulWidget[Int, Nothing, Int](
      name = "myStatefulWidget",
      initialState = 10,
      handleEvent =
        (state, events) =>
          //Only one event, which was thrown by eventThrower, is expected here.
          inside(events):
            case (event :: Nil) if event == eventToThrow =>
              Some(state + eventToThrow).pure[Update[Nothing, *]],
      render = state =>
        eventThrower(
          eventToThrow,
          drawOnly(s"State: $state"),
        ),
    )

    (
      for
        (_, updatedWidget) <- processEventAndRunUpdateForFreeWidget(widgetToTest)
      yield updatedWidget.draw
    ).value should be("State: 17")
  }

  it should "handle nested states correctly" in {
    //TODO
  }
end SimpleStatefulTest
