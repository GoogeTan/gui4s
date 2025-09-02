package gui4s.desktop.widget.library

import cats.syntax.all.*
import cats.{Functor, Monad, Monoid, Order, Traverse}
import gui4s.core.widget.Container
import gui4s.core.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import gui4s.core.widget.free.containerAsFree
import gui4s.core.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent}
import gui4s.core.widget.merge.containerMergesWithOldStates
import gui4s.core.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import gui4s.core.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

type ContainerWidget[PlacedWidget, Container[_], Place[_], Meta] = 
  (children : Container[Place[PlacedWidget]], layout : Layout[Place, Container, PlacedWidget, Meta]) => Place[PlacedWidget]

def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  C[_] : {Traverse},
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  Meta : Order,
](
  adjustDrawToMeta : (Draw, Meta) => Draw,
  adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
  isEventConsumed : Update[Boolean],
  updateListOrdered : [A : Order, B] => (list: C[A]) => (f: C[A] => Update[C[B]]) => Update[C[B]]
) : ContainerWidget[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent], C, Place, Meta] =
  type Widget_ = Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]
  (children, layout) =>
    layout(children).map(
      placedChildren =>
        Widget.ValueWrapper[
          Container[C[(Widget_, Meta)], Layout[Place, C, Widget_, Meta]],
          Update, Place, Draw, RecompositionReaction, HandleableEvent
        ](
          valueToDecorate = Container(placedChildren, layout),
          valueAsFree = containerAsFree[Place, C, Widget_, Meta](
            widgetAsFree
          ),
          valueIsDrawable = drawContainer(
            widgetWithMetaIsDrawable(
              widgetIsDrawable,
              adjustDrawToMeta
            )
          ),
          valueHandlesEvent = containerHandlesEvent[Update, Place, C, Widget_, HandleableEvent, Meta](
            childrenHandleEvent[C, Update, Place, Widget_, HandleableEvent, Meta](
              widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, HandleableEvent],
              widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent],
              isEventConsumed = isEventConsumed,
              adjustUpdateToMeta = adjustUpdateToMeta,
              updateListOrdered = updateListOrdered
            )
          ),
          valueMergesWithOldState = containerMergesWithOldStates[
            Place, C, Widget_, RecompositionReaction, Meta
          ](
            widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]
          ),
          valueReactsOnRecomposition = containerReactsOnRecomposition[
            (Widget_, Meta), C, Layout[Place, C, Widget_, Meta], RecompositionReaction
          ](
            widgetWithMetaReactsOnRecomposition[Widget_, Meta, RecompositionReaction](
              widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
            )
          ),
          valueHasInnerState = containerHasInnerStates[(Widget_, Meta), C, Layout[Place, C, Widget_, Meta], RecompositionReaction](
            widgetWithMetaHasInnerStates[Widget_, Meta, RecompositionReaction](
              widgetHasInnerStates
            )
          ),
        )
    )
end container
