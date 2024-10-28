package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import cats.data.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, EventReaction, KillTasks, Mergeable, RaiseEvent, RichTypeChecker, State, Stateful, StatefulDraw, TaskFinished, TaskResultCatcher}
import me.katze.gui4s.widget.{Widget, library}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.given

final class HighLevelApiImpl[
  Update[+_, +_]: BiMonad : CatchEvents : RaiseEvent,
  Draw : Monoid,
  Place[+_] : LabelPlacementT[LayoutPlacementMeta[MU], TextStyle] : FlatMap,
  Recompose : Monoid : KillTasks,
  WidgetTaskIn[+_],
  MU,
  -TextStyle,
  SystemEvent >: TaskFinished
](
    using val liftReaction : LiftEventReaction[Update, WidgetTaskIn[Any]],
          val linearLayoutLibrary: LayoutLibrary[Place, [A] =>> Widget[Update, Draw, Place, Recompose, A, SystemEvent], LayoutPlacementMeta[MU]]
)(
  val drawApi : SimpleDrawApi[MU, Draw],
  val placement : LayoutPlacement[Update, Draw, Place, Recompose, SystemEvent, MU]
) extends HighLevelApi with LabelApi[TextStyle] with StatefulApi with LayoutApi[MU]:
  override type WidgetTask[+T] = WidgetTaskIn[T]
  override type Widget[+Event] = Place[me.katze.gui4s.widget.Widget[Update, Draw, Place, Recompose, Event, SystemEvent]]
  
  given textDraw: LabelDraw[Draw, LayoutPlacementMeta[MU]] = (text, meta) => drawApi.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
  
  override def label(text: String, style : TextStyle): Widget[Nothing] =
    library.label(library.drawOnlyWidget, text, style)
  end label
  
  given statefulDraw : StatefulDraw[Draw] with
    override def drawStateful[T](name : String, state : State[?, T, ?], childTree: me.katze.gui4s.widget.Widget[?, Draw, ?, ?, ?, ?]): Draw = childTree.draw
  end statefulDraw
  
  override def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                            name: String,
                                                            initialState: T,
                                                            eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent]
                                                          )
                                                          (renderState: T => Widget[ChildEvent])
                                                          (
                                                            using
                                                              checkEvent : RichTypeChecker[ChildEvent],
                                                              checkState : RichTypeChecker[(T, T)]
                                                          ): Widget[ParentEvent] =
    library.stateful[Update, Draw, Place, Recompose, T, ParentEvent, ChildEvent, SystemEvent, WidgetTaskIn](name, initialState, eventHandler, renderState andThen addTaskResultCatcher[ChildEvent](using checkEvent)(name), checkState)
  end stateful

  override def column[Event](
                              children          : List[Widget[Event]],
                              verticalStrategy  : MainAxisPlacementStrategy[MU],
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
                            horizontalStrategy: MainAxisPlacementStrategy[MU],
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
                                    mainAxisStrategy      : MainAxisPlacementStrategy[MU],
                                    additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                                  )(using Widget[Event] =:= Place[widget.Widget[Update, Draw, Place, Recompose, Event, SystemEvent]]): Widget[Event] =
    linearLayoutLibrary.layout(children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
  
  
  private def addTaskResultCatcher[T](using RichTypeChecker[T])(name: String)(initial: Place[widget.Widget[Update, Draw, Place, Recompose, T, SystemEvent]]) : Place[widget.Widget[Update, Draw, Place, Recompose, T, SystemEvent]] =
    FlatMap[Place].map(initial)(TaskResultCatcher(name, Monoid[Draw].empty, _))
  end addTaskResultCatcher
end HighLevelApiImpl

  
