package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.*
import me.katze.gui4s.widget.{Widget, library}

type LayoutPlacement[Update[+_, +_], Draw, Place[+_], Recompose, DownEvent, MeasurementUnit] =
  [Event]
    => (Axis, List[Place[Widget[Update, Draw, Place, Recompose, Event, DownEvent]]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy)
    => Place[List[(Widget[Update, Draw, Place, Recompose, Event, DownEvent], LayoutPlacementMeta[MeasurementUnit])]]

final class HighLevelApiImpl[
  Update[+_, +_]: {BiMonad, CatchEvents, RaiseEvent},
  Draw : Monoid,
  Place[+_] : {TextPlacementT[LayoutPlacementMeta[MeasurementUnit], TextStyle], FlatMap},
  RecompositionIn : {Monoid},
  WidgetTaskIn[+_],
  MeasurementUnit,
  -TextStyle,
  SystemEvent >: TaskFinished
](
    using
      LiftEventReaction[Update, WidgetTaskIn[Any]],
      LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
      LabelDraw[Draw, LayoutPlacementMeta[MeasurementUnit]]
)(
    val placement : LayoutPlacement[Update, Draw, Place, RecompositionIn, SystemEvent, MeasurementUnit]
) extends HighLevelApi with LabelApi[TextStyle] with StatefulApi with LayoutApi[MeasurementUnit]:
  override type WidgetTask[+T] = WidgetTaskIn[T]
  override type Widget[+Event] = Place[me.katze.gui4s.widget.Widget[Update, Draw, Place, RecompositionIn, Event, SystemEvent]]
  override type Recomposition = RecompositionIn
  
  override def label(text: String, style : TextStyle): Widget[Nothing] =
    library.labelWidget(library.drawOnlyWidget, text, style)
  end label
  
  given statefulDraw : StatefulDraw[Draw] with
    override def drawStateful[T](name : String, state : State[?, ?, T, ?], childTree: me.katze.gui4s.widget.Widget[?, Draw, ?, ?, ?, ?]): Draw = childTree.draw
  end statefulDraw
  
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
    library.statefulWidget[Update, Draw, Place, Recomposition, T, ParentEvent, ChildEvent, SystemEvent, WidgetTask](name, initialState, dealloc_, eventHandler, render, checkState)
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

  
