package me.katze.gui4s.example
package api.impl

import api.*
import draw.given

import cats.*
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{*, given}
import me.katze.gui4s.widget.{Widget, drawOnlyWidget, textWidget}

type LayoutPlacement[Update[+_], Draw, Place[+_], Recompose, DownEvent, MeasurementUnit] =
  LayoutPlacementGeneralized[Place, MeasurementUnit, Widget[Update, Draw, Place, Recompose, DownEvent]]

type LayoutPlacementGeneralized[Place[_], MeasurementUnit, W] =
   (Axis, List[Place[W]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy)
    => Place[List[(W, LayoutPlacementMeta[MeasurementUnit])]]

final class HighLevelApiImpl[
  Update[+_, +_]: {BiMonad, CatchEvents},
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
      TextDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
     
)(
    val placement : [Event] => () => LayoutPlacement[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent, MeasurementUnit],
    val raiseEvent : [Event] => () => RaiseEvent[Update[Unit, Event]]
) extends TextWidgetApi[Place[widget.Widget[[W] =>> Update[W, Nothing], Draw, Place, Recomposition, SystemEvent]], Shaper, TextStyle]
      with StatefulApi[[Event] =>> Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]], WidgetTask, Recomposition]
      with LayoutApi[[Event] =>> Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]], MeasurementUnit]:

  type Widget[+Event] = Place[widget.Widget[[W] =>> Update[W, Event], Draw, Place, Recomposition, SystemEvent]]

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
                                  ): Widget[Event] =
    layoutWidget[[W] =>> Update[W, Event], Draw, Place, Recomposition, LayoutPlacementMeta[MeasurementUnit], SystemEvent](children, placement[Event]()(axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
  
  
  private def addTaskResultCatcher[T](using RichTypeChecker[T])(name: String)(initial: Place[widget.Widget[[W] =>> Update[W, T], Draw, Place, Recomposition, SystemEvent]]) : Place[widget.Widget[[W] =>> Update[W, T], Draw, Place, Recomposition, SystemEvent]] =
    given RaiseEvent[Update[Unit, T]] = raiseEvent[T]()
    FlatMap[Place].map(initial)(TaskResultCatcher(name, Monoid[Draw].empty, _))
  end addTaskResultCatcher
end HighLevelApiImpl

  
