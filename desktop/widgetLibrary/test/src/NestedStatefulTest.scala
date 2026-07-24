package gui4s.desktop.widget.library

import catnip.CodensityT
import catnip.syntax.all.*
import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import cats.*
import cats.arrow.FunctionK
import cats.data.OptionT
import cats.syntax.all.*
import gui4s.core.kit.*
import gui4s.core.kit.effects.*
import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.MergeStates
import org.scalatest.flatspec.AnyFlatSpec

import scala.reflect.Typeable

class NestedStatefulTest extends StatefulTests:
  override type IO[T] = Eval[T]
  type PlacedWidget[Event] = gui4s.desktop.widget.library.Widget[Update[Event, *], Place, Draw, RecompositionReaction]
  override type Widget[Event] = Place[PlacedWidget[Event]]
  override type DownEvent = Int
  override type Place[T] = OptionT[IO, T]

  type Draw = String
  type RecompositionReaction = Unit

  type UpdateTransformer[Event] =
    StateTransformer[UpdateState[List[DownEvent]]] <> EventsTransformer[List[Event]]

  override type Update[Event, T] = UpdateTransformer[Event][IO, T]

  override given updateIsMonadForallEvents[Event]: Monad[Update[Event, *]] =
    summon

  val noRecomposition: RecompositionReaction = ()

  def runPlace: OptionT[Eval, *] ~> Eval = FunctionK.lift(
    [T] => (place: Place[T]) =>
      Eval.later(
        inside(place.value.value):
          case Some(v) => v
      )
  )

  def runRecomposition: Unit => Eval[Unit] = _ => Eval.Unit

  def runUpdate[Event, T](downEvents: List[DownEvent], update: Update[Event, T]): IO[(List[Event], T)] =
    runUpdate(UpdateState(downEvents), update)
  end runUpdate


  def runUpdate[Event, T](state: UpdateState[List[DownEvent]], update: Update[Event, T]): IO[(List[Event], T)] =
    update.runA(state).run
  end runUpdate

  def liftUpdate[OwnEvent, ChildEvent]: [T] => (fa: Update[ChildEvent, T]) => Update[OwnEvent, (List[ChildEvent], T)] =
    [T] => (fa: Update[ChildEvent, T]) =>
      EventsTransformer.catchEvents(fa)
  end liftUpdate


  def widgetsAreMergeable[Event] =
    widgetsCanUpdateStateFromTheOldOnes[PlacedWidget[Event], Place, Id, RecompositionReaction](
      widgetMergesWithOldState,
      widgetHasInnerStates
    )
  end widgetsAreMergeable

  def throwEvents[Event](events: List[Event]): Update[Event, Unit] =
    EventsTransformer.raiseEvents(events)
  end throwEvents

  //TODO extract this into widget library module to reuse with same code in desktop kit
  def typeCheckState[State : Typeable](error : [T] => Any => Place[T]) =
    [T] => (a: Any, f: StatefulState[State] => Option[Place[T]]) =>
      a match
        case StatefulState(initial : State, current: State) =>
          f(StatefulState(initial, current))
        case _ => Some(error(a))
  end typeCheckState

  override def statefulWidget[State: Typeable, OwnEvent, ChildEvent](
    name: String,
    initialState: Place[State],
    stateAsFree: State => Place[Option[State]],
    handleEvent: HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render: State => Widget[ChildEvent],
    mergeStates: MergeStates[Place, State]
  ): Widget[OwnEvent] =
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

  override def simpleStatefulWidget[State: {Eq, Typeable}, OwnEvent, ChildEvent](
    name: String,
    initialState: State,
    handleEvent: HandlesEventF[State, List[ChildEvent], Update[OwnEvent, *] * Option],
    render: State => Widget[ChildEvent],
  ): Widget[OwnEvent] =
    statefulWidget(name, initialState.pure, _ => None.pure, handleEvent, render, MergeStates.default([T] => _ => OptionT.none))
  end simpleStatefulWidget

  override def insideIO[T](value: IO[T])(f: PartialFunction[T, Unit]): Unit =
    f.applyOrElse(value.value, _ => ())
  end insideIO

  override def drawOnly[Event](rendersTo: String): Widget[Event] =
    constanctSizeDrawOnlyWidget[Update[Event, *], Place, Id, Draw, RecompositionReaction](rendersTo, noRecomposition)
  end drawOnly

  override def eventThrower[Event](eventToThrow: Event, body: Widget[Event]): Widget[Event] =
    gui4s.desktop.widget.library.decorator.updateDecorator[
      Update[Event, *],
      Place,
      Draw,
      RecompositionReaction
    ](
      oldHandler => widget =>
        oldHandler(widget) <* throwEvents(List(eventToThrow))
    )(body)
  end eventThrower

  def processEvent[Event](testWidget : PlacedWidget[Event]) : Update[Event, IO[PlacedWidget[Event]]] =
    gui4s.desktop.widget.library.processEvent(runPlace, runRecomposition, testWidget)
  end processEvent

  override def update[Event](
    widget: Widget[Event],
    downEvents: List[DownEvent]
  ): IO[(List[Event], Widget[Event])] =
    runUpdate(downEvents, processEvent(runPlace(widget).value))
      .flatMap((events, ioWidget) => ioWidget.map(widget => (events, widgetAsFree(widget))))
  end update

  extension [Event](widget: Widget[Event])
    override infix def shouldDrawInto(str: String): Unit =
      runPlace(widget).value.draw should be(str)
    end shouldDrawInto
  end extension
end NestedStatefulTest