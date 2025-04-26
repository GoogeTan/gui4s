package me.katze.gui4s.example
package api.impl

import api.*
import draw.given

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{*, given}
import me.katze.gui4s.widget.{Widget, drawOnlyWidget, textWidget}


final class TextApi[
  Update[+_, +_]: BiMonad,
  Draw,
  Place[+_] : {TextPlacementT[Shaper, TextStyle, LayoutPlacementMeta[MeasurementUnit]], Functor},
  Recomposition : Empty,
  MeasurementUnit,
  -TextStyle,
  -Shaper,
  SystemEvent,
](
  using TextDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
) extends TextWidgetApi[Place[widget.Widget[[W] =>> Update[W, Nothing], Draw, Place, Recomposition, SystemEvent]], Shaper, TextStyle]:
  override def text(text: String, shaper: Shaper, style: TextStyle): Place[Widget[[W] =>> Update[W, Nothing], Draw, Place, Recomposition, SystemEvent]] =
    textWidget(drawOnlyWidget, text, shaper, style)
  end text
end TextApi

final class StatefulApi_[
  Update[+_, +_]: {BiMonad, CatchEvents, LiftEventReaction},
  Draw : Monoid,
  Place[+_] : FlatMap,
  Recomposition,
  WidgetTask[+_] : Functor,
  SystemEvent >: TaskFinished,
  Supervisor,
](
                           val raiseEvent : [Event] => () => RaiseEvent[Update[Unit, Event]],
                           val runOnSupervisor : (Supervisor, WidgetTask[Any]) => Update[Unit, Nothing]
) extends StatefulApi[[Event] =>> Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]], WidgetTask, Recomposition, Supervisor]:
  private type Widget[+Event] = Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]]
  override def stateful[State: {Equiv, RichTypeChecker}, ParentEvent, ChildEvent: RichTypeChecker](
                                                                                                    name: String,
                                                                                                    initialState: State,
                                                                                                    dealloc_ : State => Recomposition,
                                                                                                    eventHandler: (State, ChildEvent) => EventReaction[State, ParentEvent, WidgetTask[ChildEvent]],
                                                                                                    supervisor: Supervisor
                                                                                                  )(renderState: State => Widget[ChildEvent]): Widget[ParentEvent] =
    val render = renderState andThen addTaskResultCatcher[ChildEvent](name)
    statefulWidget[Update, Draw, Place, Recomposition, State, ParentEvent, ChildEvent, SystemEvent, WidgetTask](name, initialState, dealloc_, eventHandler, render, runEventsOnSupervisor(supervisor))
  end stateful

  override def stateInBetween[
    State: {Equiv, RichTypeChecker},
    TransitiveEvent: RichTypeChecker,
    OwnEvent: RichTypeChecker
  ](
     name: String,
     initialState: State,
     dealloc: State => Recomposition,
     eventHandler: (State, OwnEvent) => EventReaction[State, TransitiveEvent, WidgetTask[OwnEvent]],
     supervisor: Supervisor
   )(renderState: State => Widget[Either[TransitiveEvent, OwnEvent]]): Widget[TransitiveEvent] =
    stateful[State, TransitiveEvent, Either[TransitiveEvent, OwnEvent]](
      name,
      initialState,
      dealloc,
      (state, event) =>
        event match
          case Left(transitiveEvent) => EventReaction(state, List(transitiveEvent), Nil)
          case Right(ownEvent) => eventHandler(state, ownEvent).mapIOS(_.map(Right(_))),
      supervisor
    )(renderState)
  end stateInBetween


  def runEventsOnSupervisor(supervisor: Supervisor)(events: List[WidgetTask[Any]]): Update[Unit, Nothing] =
    events.traverse_[[A] =>> Update[A, Nothing], Unit](runOnSupervisor(supervisor, _))

  private def addTaskResultCatcher[T : RichTypeChecker](name: String)(initial: Place[widget.Widget[[W] =>> Update[W, T], Draw, Place, Recomposition, SystemEvent]]) : Place[widget.Widget[[W] =>> Update[W, T], Draw, Place, Recomposition, SystemEvent]] =
    given RaiseEvent[Update[Unit, T]] = raiseEvent[T]()
    Functor[Place].map(initial)(TaskResultCatcher(name, Monoid[Draw].empty, _))
  end addTaskResultCatcher
end StatefulApi_


type LayoutPlacementGeneralized[Place[_], MeasurementUnit, PlacementMeta, Widget] =
   (Axis, List[Place[Widget]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy) => Place[List[(Widget, PlacementMeta)]]

final class LayoutApi_[
  Update[+_, +_]: BiMonad,
  Draw : Monoid,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  MeasurementUnit,
  PlacementMeta,
  SystemEvent,
](
    using LayoutDraw[Draw, PlacementMeta],
)(
  val placement : [Event] => (Axis, List[Place[Widget[[A] =>> Update[A, Event], Draw, Place, Recomposition, SystemEvent]]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy) => 
    Place[List[(Widget[[A] =>> Update[A, Event], Draw, Place, Recomposition, SystemEvent], PlacementMeta)]],
) extends  LayoutApi[[Event] =>> Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]], MeasurementUnit]:

  private type Widget[+Event] = Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]]

  override def column[Event](
                              children          : List[Widget[Event]],
                              verticalStrategy  : MainAxisPlacementStrategy[MeasurementUnit],
                              horizontalStrategy: AdditionalAxisPlacementStrategy
                            ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Vertical,
      mainAxisStrategy = verticalStrategy,
      additionalAxisStrategy = horizontalStrategy,
    )
  end column

  override def row[Event](
                            children          : List[Widget[Event]],
                            horizontalStrategy: MainAxisPlacementStrategy[MeasurementUnit],
                            verticalStrategy  : AdditionalAxisPlacementStrategy
                          ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Horizontal,
      mainAxisStrategy = horizontalStrategy,
      additionalAxisStrategy = verticalStrategy,
    )
  end row

  private def linearLayout[Event](
                                    children              : List[Widget[Event]],
                                    axis                  : Axis,
                                    mainAxisStrategy      : MainAxisPlacementStrategy[MeasurementUnit],
                                    additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                                  ): Widget[Event] =
    layoutWidget[[W] =>> Update[W, Event], Draw, Place, Recomposition, PlacementMeta, SystemEvent](children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
end LayoutApi_

  
