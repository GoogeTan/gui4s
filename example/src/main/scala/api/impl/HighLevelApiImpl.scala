package me.katze.gui4s.example
package api.impl

import api.*
import draw.*

import cats.*
import cats.data.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.*
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.rowcolumn.*
import me.katze.gui4s.widget.impl.FreeStatefulFabricImpl
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker, State, StatefulDraw}
import me.katze.gui4s.widget.{PlacedWidget, library}

trait HighLevelApiImpl[
  F[+_],
  Draw[_],
  PlacementEffect[+_] : LabelPlacementT[LayoutPlacementMeta[MU], TextStyle],
  MU,
  -TextStyle,
](
  using val wl : WidgetLibraryImpl[F, Draw[Unit], PlacementEffect]
)(
  val drawApi : SimpleDrawApi[MU, Draw[Unit]]
) extends HighLevelApi with LabelApi[TextStyle] with StatefulApi:
  override type WidgetTask[+T] = wl.WidgetTask[T]
  override type Widget[+Event] = wl.Widget[Event]
  
  given textDraw: LabelDraw[Draw[Unit], LayoutPlacementMeta[MU]] = (text, meta) => drawApi.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
  
  override def label(text: String, style : TextStyle): Widget[Nothing] =
    library.label(using wl, textDraw, summon, library.drawOnlyWidget)(text, style)
  end label
  
  given statefulDraw : StatefulDraw[wl.Draw] with
    override def drawStateful[T](name : String, state : State[Any, T, Any, Any], childTree: PlacedWidget[wl.Draw, Any, [A, B] =>> Any, Any, Nothing]): wl.Draw = childTree.draw
  end statefulDraw
  
  override def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                            name: String,
                                                            initialState: T,
                                                            eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ChildEvent, ParentEvent]
                                                          )
                                                          (renderState: T => Widget[ChildEvent])
                                                          (
                                                            using
                                                            RichTypeChecker[ChildEvent],
                                                            RichTypeChecker[(T, T)]
                                                          ): Widget[ParentEvent] =
    library.stateful(
      using wl
    )(
      using FreeStatefulFabricImpl[wl.Draw, wl.WidgetTask, wl.PlacedWidget, wl.PlacementEffect, ParentEvent, wl.SystemEvent, ChildEvent, wl.SystemEvent](wl.constructRealWidget)(using statefulDraw, wl.placementIsEffect, wl.freeTreesAreMergeable)
    )(
      name, initialState, eventHandler , renderState
    )
  end stateful


end HighLevelApiImpl

  
