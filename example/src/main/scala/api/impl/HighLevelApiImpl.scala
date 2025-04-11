package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.*
import me.katze.gui4s.widget.{Widget, drawOnlyWidget, textWidget}
import me.katze.gui4s.example.draw.given

type LayoutPlacement[Update[+_, +_], Draw, Place[+_], Recompose, DownEvent, MeasurementUnit] =
  LayoutPlacementGeneralized[Place, MeasurementUnit, [Event] =>> Widget[Update, Draw, Place, Recompose, Event, DownEvent]]

type LayoutPlacementGeneralized[Place[_], MeasurementUnit, W[_]] =
  [Event]
    => (Axis, List[Place[W[Event]]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy)
    => Place[List[(W[Event], LayoutPlacementMeta[MeasurementUnit])]]

final class HighLevelApiImpl[
  Update[+_, +_]: {BiMonad, CatchEvents, RaiseEvent},
  Draw : Monoid,
  Place[+_] : {TextPlacementT[Shaper, TextStyle, LayoutPlacementMeta[MeasurementUnit]], FlatMap},
  Recomposition : {Monoid},
  WidgetTask[+_],
  MeasurementUnit,
  -TextStyle,
  -Shaper,
  SystemEvent >: TaskFinished
](
    using
      LiftEventReaction[Update, WidgetTask[Any]],
      LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
      TextDraw[Draw, LayoutPlacementMeta[MeasurementUnit]]
)(
    val placement : LayoutPlacement[Update, Draw, Place, Recomposition, SystemEvent, MeasurementUnit]
) extends TextWidgetApi[Place[widget.Widget[Update, Draw, Place, Recomposition, Nothing, SystemEvent]], Shaper, TextStyle]
      with StatefulApi[[Event] =>> Place[widget.Widget[Update, Draw, Place, Recomposition, Event, SystemEvent]], WidgetTask, Recomposition]
      with LayoutApi[[Event] =>> Place[widget.Widget[Update, Draw, Place, Recomposition, Event, SystemEvent]], MeasurementUnit]:

  type Widget[+Event] = Place[widget.Widget[Update, Draw, Place, Recomposition, Event, SystemEvent]]

  override def text(text: String, shaper : Shaper, style : TextStyle): Widget[Nothing] =
    textWidget(drawOnlyWidget, text, shaper, style)
  end text
  
  override def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                            name: String,
                                                            initialState: T,
                                                            dealloc_ : T => Recomposition,
                                                            eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent]
                                                          )
                                                          (renderState: T => Widget[ChildEvent])
                                                          (
                                                            using
                                                              checkEvent : RichTypeChecker[ChildEvent],
                                                              checkState : RichTypeChecker[(T, T)]
                                                          ): Widget[ParentEvent] =
    val render = renderState andThen addTaskResultCatcher[ChildEvent](using checkEvent)(name)
    statefulWidget[Update, Draw, Place, Recomposition, T, ParentEvent, ChildEvent, SystemEvent, WidgetTask](name, initialState, dealloc_, eventHandler, render, checkState)
  end stateful

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
                                  )(using Widget[Event] =:= Place[widget.Widget[Update, Draw, Place, Recomposition, Event, SystemEvent]]): Widget[Event] =
    layoutWidget[Update, Draw, Place, Recomposition, LayoutPlacementMeta[MeasurementUnit], Event, SystemEvent](children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
  
  
  private def addTaskResultCatcher[T](using RichTypeChecker[T])(name: String)(initial: Place[widget.Widget[Update, Draw, Place, Recomposition, T, SystemEvent]]) : Place[widget.Widget[Update, Draw, Place, Recomposition, T, SystemEvent]] =
    FlatMap[Place].map(initial)(TaskResultCatcher(name, Monoid[Draw].empty, _))
  end addTaskResultCatcher
end HighLevelApiImpl

  
