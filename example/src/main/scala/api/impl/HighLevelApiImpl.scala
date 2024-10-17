package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import cats.data.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, EventReaction, Mergeable, RichTypeChecker, State, Stateful, StatefulDraw, TaskFinished}
import me.katze.gui4s.widget.{PlacedWidget, library}

trait HighLevelApiImpl[
  UpdateIn[+_, +_]: BiMonad : CatchEvents, 
  F[+_],
  Draw[_],
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
  given wl.placementIsEffect.type = wl.placementIsEffect
  given[A, B]: Mergeable[wl.FreeWidget[A, B]] = wl.freeTreesAreMergeable
  
  override type WidgetTask[+T] = WidgetTaskIn[T]
  override type Widget[+Event] = wl.Widget[Event]
  
  given textDraw: LabelDraw[Draw[Unit], LayoutPlacementMeta[MU]] = (text, meta) => drawApi.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
  
  override def label(text: String, style : TextStyle): Widget[Nothing] =
    library.label(using wl, textDraw, summon, library.drawOnlyWidget)(text, style)
  end label
  
  given statefulDraw : StatefulDraw[wl.Draw] with
    override def drawStateful[T](name : String, state : State[?, Any, T, Any, Any], childTree: PlacedWidget[?, wl.Draw, ?, ?, ?]): wl.Draw = childTree.draw
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
    library.stateful[T, ParentEvent, ChildEvent, WidgetTaskIn](using wl)(using freeStatefulFabricImpl, liftReaction)(name, initialState, eventHandler , renderState)
  end stateful

  def freeStatefulFabricImpl[
    RaiseableEvent, HandleableEvent >: TaskFinished,
    ChildRaiseableEvent : RichTypeChecker, ChildHandleableEvent >: HandleableEvent
  ](
    name     : String,
    state    : State[[W] =>> wl.Update[W, RaiseableEvent], WidgetTaskIn[ChildRaiseableEvent], ChildRaiseableEvent, RaiseableEvent, wl.FreeWidget[ChildRaiseableEvent, ChildHandleableEvent]],
    childTree: wl.FreeWidget[ChildRaiseableEvent, ChildHandleableEvent]
  ): wl.FreeWidget[RaiseableEvent, HandleableEvent] =
    given this.type = this
    wl.placementIsEffect.map(childTree)(placedChildTree =>
      Stateful[
        wl.Update,
        wl.Draw,
        WidgetTaskIn,
        [A, B] =>> wl.FreeWidget[A, B],
        [A, B] =>> wl.PlacedWidget[A, B],
        RaiseableEvent, HandleableEvent,
        ChildRaiseableEvent, ChildHandleableEvent
      ](name, state, placedChildTree)(using summon, summon, statefulDraw, wl.freeTreesAreMergeable, freeStatefulFabricImpl, summon[RichTypeChecker[ChildRaiseableEvent]])
    ).map(wl.constructRealWidget)
  end freeStatefulFabricImpl
end HighLevelApiImpl

  
