package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import cats.data.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, EventReaction, Mergeable, RaiseEvent, RichTypeChecker, State, Stateful, StatefulDraw, TaskFinished, TaskResultCatcher}
import me.katze.gui4s.widget.{PlacedWidget, library}

trait HighLevelApiImpl[
  UpdateIn[+_, +_]: BiMonad : CatchEvents : RaiseEvent,
  F[+_],
  Draw[_] : Applicative,
  PlacementEffect[+_] : LabelPlacementT[LayoutPlacementMeta[MU], TextStyle],
  WidgetTaskIn[+_],
  MU,
  -TextStyle,
  SystemEvent >: TaskFinished
](
    using val 
      wl : WidgetLibraryImpl[UpdateIn, Draw[Unit], PlacementEffect, SystemEvent],
      liftReaction : LiftEventReaction[UpdateIn, WidgetTaskIn[Any]]
)(
  val drawApi : SimpleDrawApi[MU, Draw[Unit]]
) extends HighLevelApi with LabelApi[TextStyle] with StatefulApi:
  given FlatMap[PlacementEffect] = wl.placementIsEffect
  given[A, B]: Mergeable[wl.FreeWidget[A, B]] = wl.freeTreesAreMergeable
  
  override type WidgetTask[+T] = WidgetTaskIn[T]
  override type Widget[+Event] = wl.Widget[Event]
  
  given textDraw: LabelDraw[Draw[Unit], LayoutPlacementMeta[MU]] = (text, meta) => drawApi.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
  
  override def label(text: String, style : TextStyle): Widget[Nothing] =
    library.label(using wl, textDraw, summon, library.drawOnlyWidget)(text, style)
  end label
  
  given statefulDraw : StatefulDraw[wl.Draw] with
    override def drawStateful[T](name : String, state : State[?, T, ?], childTree: PlacedWidget[?, wl.Draw, ?, ?, ?]): wl.Draw = childTree.draw
  end statefulDraw
  
  override def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                            name: String,
                                                            initialState: T,
                                                            eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent]
                                                          )
                                                          (renderState: T => Widget[ChildEvent])
                                                          (
                                                            using
                                                            RichTypeChecker[ChildEvent],
                                                            RichTypeChecker[(T, T)]
                                                          ): Widget[ParentEvent] =
    library.stateful[T, ParentEvent, ChildEvent, WidgetTaskIn](using wl)(using liftReaction)(freeStatefulFabricImpl, name, initialState, eventHandler, renderState andThen addTaskResultCatcher[ChildEvent](name))
  end stateful

  def addTaskResultCatcher[T : RichTypeChecker](name: String)(initial: Widget[T]) : Widget[T] =
    wl.placementIsEffect
      .map(initial)(TaskResultCatcher(name, ().pure[Draw], _, wl.constructRealWidget))
      .map(wl.constructRealWidget)
  end addTaskResultCatcher

  def freeStatefulFabricImpl[
    RaiseableEvent, HandleableEvent,
    ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
  ](
    name     : String,
    state    : State[[W] =>> wl.Update[W, RaiseableEvent], ChildRaiseableEvent, wl.FreeWidget[ChildRaiseableEvent, ChildHandleableEvent]],
    childTree: wl.FreeWidget[ChildRaiseableEvent, ChildHandleableEvent]
  ): wl.FreeWidget[RaiseableEvent, HandleableEvent] =
    wl.placementIsEffect.map(childTree)(placedChildTree =>
      Stateful[
        wl.Update,
        wl.Draw,
        [A, B] =>> wl.FreeWidget[A, B],
        [A, B] =>> wl.PlacedWidget[A, B],
        RaiseableEvent, HandleableEvent,
        ChildRaiseableEvent, ChildHandleableEvent
      ](name, state, placedChildTree)(using summon, summon, statefulDraw, wl.freeTreesAreMergeable, freeStatefulFabricImpl)
    ).map(wl.constructRealWidget)
  end freeStatefulFabricImpl
end HighLevelApiImpl

  
