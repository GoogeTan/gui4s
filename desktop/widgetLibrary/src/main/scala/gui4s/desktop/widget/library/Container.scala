package gui4s.desktop.widget.library

import cats.{Functor, Monad, Monoid, Traverse}
import cats.syntax.all.*
import gui4s.core.widget.Container
import gui4s.core.widget.free.containerAsFree
import gui4s.core.widget.handle.{Layout, TraverseChildrenOrdered, childrenHandleEvent, containerHandlesEvent}
import gui4s.core.widget.library.*
import gui4s.core.widget.merge.containerMergesWithOldStates
import gui4s.core.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import gui4s.core.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  Collection[_] : Traverse,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  Meta,
](
  adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
  isEventConsumed : Update[Boolean],
  updateContainerOrdered : TraverseChildrenOrdered[
    Update,
    Place,
    Collection,
    Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
    Meta
  ],
  drawOrdered : Collection[(Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent], Meta)] => Draw,
) : ContainerWidget[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent], Collection, Place, Meta] =
  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
  (children, layout) =>
    layout(children).map(
      placedChildren =>
        Widget.ValueWrapper[
          Container[Collection[(PlacedWidget, Meta)], Layout[Place, Collection, PlacedWidget, Meta]],
          Update, Place, Draw, RecompositionReaction, EnvironmentalEvent
        ](
          valueToDecorate = Container(placedChildren, layout),
          valueAsFree = containerAsFree[Place, Collection, PlacedWidget, Meta](
            widgetAsFree
          ),
          valueIsDrawable = container => drawOrdered(container.children),
          valueHandlesEvent = containerHandlesEvent[Update, Place, Collection, PlacedWidget, EnvironmentalEvent, Meta](
            childrenHandleEvent[Collection, Update, Place, PlacedWidget, EnvironmentalEvent, Meta](
              widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
              widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
              isEventConsumed = isEventConsumed,
              adjustUpdateToMeta = adjustUpdateToMeta,
              traverseContainerOrdered = updateContainerOrdered(_)
            )
          ),
          valueMergesWithOldState = containerMergesWithOldStates[
            Place, Collection, PlacedWidget, RecompositionReaction, Meta
          ](
            widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
          ),
          valueReactsOnRecomposition = containerReactsOnRecomposition[
            (PlacedWidget, Meta), Collection, Layout[Place, Collection, PlacedWidget, Meta], RecompositionReaction
          ](
            widgetWithMetaReactsOnRecomposition[PlacedWidget, Meta, RecompositionReaction](
              widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
            )
          ),
          valueHasInnerState = containerHasInnerStates[(PlacedWidget, Meta), Collection, Layout[Place, Collection, PlacedWidget, Meta], RecompositionReaction](
            widgetWithMetaHasInnerStates[PlacedWidget, Meta, RecompositionReaction](
              widgetHasInnerStates
            )
          ),
        )
    )
end container
