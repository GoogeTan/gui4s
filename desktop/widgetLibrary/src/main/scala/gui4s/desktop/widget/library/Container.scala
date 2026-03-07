package gui4s.desktop.widget.library

import catnip.Zip
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid, Traverse}
import gui4s.core.widget.free.containerAsFree
import gui4s.core.widget.handle.{Layout, TraverseChildrenOrdered, childrenHandleEvent, containerHandlesEvent}
import gui4s.core.widget.merge.containerMergesWithOldStates
import gui4s.core.widget.recomposition.{containerReactsOnRecomposition, widgetWithMetaReactsOnRecomposition}
import gui4s.core.widget.state.{containerHasInnerStates, widgetWithMetaHasInnerStates}

def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  Collection[_] : {Traverse, Zip},
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  Meta,
  FreeChildren,
  IncrementalFreeChildren
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
  children: Collection[FreeChildren],
  layout: Layout[
    Place,
    Collection,
    FreeChildren,
    IncrementalFreeChildren,
    (Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent], Meta)
  ],
  incrementalFreeChildrenFromPlaced: (
    (Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent], Meta),
    Option[Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]]]
  ) => IncrementalFreeChildren
) : Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]] =
  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
  type AppliedLayout = Layout[
    Place,
    Collection,
    FreeChildren,
    IncrementalFreeChildren,
    (Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent], Meta)
  ]
  layout.place(children).map(
    placedChildren =>
      Widget.ValueWrapper[
        Collection[(PlacedWidget, Meta)],
        Update, Place, Draw, RecompositionReaction, EnvironmentalEvent
      ](
        valueToDecorate = placedChildren,
        valueAsFree = containerAsFree[Place, Collection, PlacedWidget, Meta](
           children => layout.placeIncrementally(children.map(incrementalFreeChildrenFromPlaced(_, None)))
        ),
        valueIsDrawable = children => drawOrdered(children),
        valueHandlesEvent = containerHandlesEvent[Update, Place, Collection, PlacedWidget, EnvironmentalEvent, Meta](
          childrenHandleEvent[Collection, Update, Place, PlacedWidget, EnvironmentalEvent, Meta](
            widgetHandlesEvent = widgetHandlesEvent[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
            widgetAsFree = widgetAsFree[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
            isEventConsumed = isEventConsumed,
            adjustUpdateToMeta = adjustUpdateToMeta,
            traverseContainerOrdered = updateContainerOrdered(_)
          ),
          children =>
            layout.placeIncrementally(
              children.map(incrementalFreeChildrenFromPlaced(_, _))
            )
        ),
        valueMergesWithOldState = containerMergesWithOldStates[
          Place, Collection, PlacedWidget, RecompositionReaction, Meta
        ](
          widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent],
          children =>
            layout.placeIncrementally(
              children.map(incrementalFreeChildrenFromPlaced(_, _))
            )
        ),
        valueReactsOnRecomposition = containerReactsOnRecomposition[
          (PlacedWidget, Meta), Collection, RecompositionReaction
        ](
          widgetWithMetaReactsOnRecomposition[PlacedWidget, Meta, RecompositionReaction](
            widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
          )
        ),
        valueHasInnerState = containerHasInnerStates[(PlacedWidget, Meta), Collection, RecompositionReaction](
          widgetWithMetaHasInnerStates[PlacedWidget, Meta, RecompositionReaction](
            widgetHasInnerStates
          )
        ),
      )
  )
end container
