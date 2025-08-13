package me.katze.gui4s.widget.library

import cats.Id
import cats.data.{NonEmptyList, Writer}
import cats.syntax.all.*
import me.katze.gui4s.widget.{Path, Stateful, StatefulBehaviour, StatefulState, given}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import catnip.syntax.all.{*, given}

import scala.io.Codec.fallbackSystemCodec.name
import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
final class StatefulTest extends AnyFlatSpec with Matchers:
  type Update[Event, Value] = Writer[List[Event], Value]

  enum PlaceError:
    case StateCheckError(path : Path, state : Any)
    
  type Place[Value] = Either[PlaceError, Value]
  type Draw = String
  type RecompositionReaction = Unit
  type HandleableEvent = String

  type TestWidget[T, Event] = Widget.ValueWrapper[T, Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent]
  type TestWidgetUntyped[Event] = Widget[Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent]
  type FreeTestWidget[T, Event] = Place[TestWidget[T, Event]]
  type FreeTestWidgetUntyped[Event] = Place[TestWidgetUntyped[Event]]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def typeCheckState[State : Typeable]: [T] => (Any, Path, StatefulState[State] => Place[T]) => Place[T] =
    [T] => (value: Any, path: Path, callback: StatefulState[State] => Place[T]) =>
      value match
        case StatefulState(initial: State, current: State) => callback(StatefulState(initial, current))
        case _ => Left(PlaceError.StateCheckError(path, value))
      end match
  end typeCheckState

  def statefulWidget[
    State : {Equiv, Typeable},
    ParentEvent,
    ChildEvent
  ](
    name : String,
    initialState : State,
    eventHandler : (State, Path, NonEmptyList[ChildEvent]) => Update[ParentEvent, State],
    body : State => FreeTestWidgetUntyped[ChildEvent],
    destructor : State => RecompositionReaction
  ) : FreeTestWidget[
    Stateful[
      TestWidgetUntyped[ChildEvent],
      StatefulBehaviour[
        State,
        State => FreeTestWidgetUntyped[ChildEvent],
        (State, Path, NonEmptyList[ChildEvent]) => Update[ParentEvent, State],
        State => RecompositionReaction
      ]
    ],
    ParentEvent
  ] =
    stateful[
      Update, 
      Place, 
      Draw,
      RecompositionReaction, 
      HandleableEvent, 
      State, 
      ParentEvent, 
      ChildEvent
    ](
      widgetsAreMergable[Update[ChildEvent, *], Place, Id, Draw, RecompositionReaction, HandleableEvent], 
      typeCheckState
    )(name, initialState, eventHandler, body, destructor)
  end statefulWidget

  def leaf[Event] : FreeTestWidget[String, Event] =
    leafWidget[String, Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent](Right("leaf"), "leaf", ())

  extension[T, Event](value : FreeTestWidget[T, Event])
    def onEvent(f: (Path, HandleableEvent) => List[Event]) : FreeTestWidget[T, Event] =
      value
        .map(
          eventCatcher(().pure[Update[Event, *]])(_)((path, event) => Writer.tell(f(path, event)).as(false))
        )
    end onEvent
  end extension

  def eventRaiser[Event](f : HandleableEvent => Event) : FreeTestWidget[String, Event] =
    leaf.onEvent((_, event) => List(f(event)))
  end eventRaiser

  def handleEvent[T, Event](freeTestWidget : FreeTestWidget[T, Event], handleableEvent : HandleableEvent) : Place[Update[Event, FreeTestWidget[T, Event]]] =
    freeTestWidget.map(
      widgetHandlesEventTyped[T, Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent](_, new Path(), handleableEvent)
    )
  end handleEvent

  // TODO а какие тесты тут вообще могут быть? Будто бы тесты в widget все покрывают и так
end StatefulTest
