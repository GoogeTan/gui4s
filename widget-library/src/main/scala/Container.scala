package me.katze.gui4s.widget.library

import catnip.syntax.all.given
import cats.syntax.all.*
import cats.{Applicative, Foldable, Functor, Monad, Monoid, Order, SemigroupK, Traverse}
import me.*
import me.katze.gui4s.widget.Container
import me.katze.gui4s.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import me.katze.gui4s.widget.free.containerAsFree
import me.katze.gui4s.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent}
import me.katze.gui4s.widget.merge.containerMergesWithOldStates
import me.katze.gui4s.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import me.katze.gui4s.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

type ContainerWidget[PlacedWidget, Container[_], Place[_], Meta] = (children : Container[Place[PlacedWidget]], layout : Layout[Place, Container, PlacedWidget, Meta]) => Place[PlacedWidget]

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
