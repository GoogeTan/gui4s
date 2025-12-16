package gui4s.desktop.widget.library

import catnip.syntax.all.*
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid, Order, Traverse}
import gui4s.core.kit.widget.*
import gui4s.core.widget.Container
import gui4s.core.widget.draw.{drawContainer, widgetWithMetaIsDrawable}
import gui4s.core.widget.free.containerAsFree
import gui4s.core.widget.handle.{Layout, childrenHandleEvent, containerHandlesEvent}
import gui4s.core.widget.merge.containerMergesWithOldStates
import gui4s.core.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import gui4s.core.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  C[_] : Traverse,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  Meta : Order,
](
  adjustDrawToMeta : (Draw, Meta) => Draw,
  adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
  isEventConsumed : Update[Boolean],
  traverseContainerOrdered : TraverseOrdered[Update, C]
) : ContainerWidget[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent], C, Place, Meta] =
  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]
  given Order[(PlacedWidget, Meta)] = Order.by(_._2)
  (children, layout) =>
    layout(children).map(
      placedChildren =>
        Widget.ValueWrapper[
          Container[C[(PlacedWidget, Meta)], Layout[Place, C, PlacedWidget, Meta]],
          Update, Place, Draw, RecompositionReaction, HandleableEvent
        ](
          valueToDecorate = Container(placedChildren, layout),
          valueAsFree = containerAsFree[Place, C, PlacedWidget, Meta](
            widgetAsFree
          ),
          valueIsDrawable = drawContainer(
            widgetWithMetaIsDrawable(
              widgetIsDrawable,
              adjustDrawToMeta
            )
          ),
          valueHandlesEvent = containerHandlesEvent[Update, Place, C, PlacedWidget, HandleableEvent, Meta](
            childrenHandleEvent[C, Update, Place, PlacedWidget, HandleableEvent, Meta](
              widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, HandleableEvent],
              widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent],
              isEventConsumed = isEventConsumed,
              adjustUpdateToMeta = adjustUpdateToMeta,
              traverseContainerOrdered = traverseContainerOrdered
            )
          ),
          valueMergesWithOldState = containerMergesWithOldStates[
            Place, C, PlacedWidget, RecompositionReaction, Meta
          ](
            widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]
          ),
          valueReactsOnRecomposition = containerReactsOnRecomposition[
            (PlacedWidget, Meta), C, Layout[Place, C, PlacedWidget, Meta], RecompositionReaction
          ](
            widgetWithMetaReactsOnRecomposition[PlacedWidget, Meta, RecompositionReaction](
              widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent]
            )
          ),
          valueHasInnerState = containerHasInnerStates[(PlacedWidget, Meta), C, Layout[Place, C, PlacedWidget, Meta], RecompositionReaction](
            widgetWithMetaHasInnerStates[PlacedWidget, Meta, RecompositionReaction](
              widgetHasInnerStates
            )
          ),
        )
    )
end container
