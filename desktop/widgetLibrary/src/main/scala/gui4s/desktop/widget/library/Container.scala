package gui4s.desktop.widget.library

import catnip.Zip
import cats.Functor
import cats.Monad
import cats.Monoid
import cats.Traverse
import cats.syntax.all._

import gui4s.core.widget.free.containerAsFree
import gui4s.core.widget.handle.HandlesEvent
import gui4s.core.widget.handle.TraverseChildrenOrdered
import gui4s.core.widget.handle.childrenHandleEvent
import gui4s.core.widget.handle.containerHandlesEvent
import gui4s.core.widget.merge.MergesWithOldStates
import gui4s.core.widget.merge.containerMergesWithOldStates
import gui4s.core.widget.recomposition.ReactsOnRecomposition
import gui4s.core.widget.recomposition.containerReactsOnRecomposition
import gui4s.core.widget.state.HasInnerStates
import gui4s.core.widget.state.containerHasInnerStates

/**
 * Тип функции, описывающей установку множества виджетов в контейнер.
 * Принимает множество свободных детей и возвращает свободное множество размещенных виджетов.
 *
 * @tparam Place Эффект установки виджета
 * @tparam Collection Множества виджетов. Это может быть List, если это правило установки линейного контейнера или Id, если правило только для одного виджета.:
 * @tparam SizedWidget Размещенный виджет
 * @tparam Meta Вспомогательные данные об результатах установки(например, координаты). TODO может, можно обобщить на произвольную комонаду
 */
type Layout2[
  Place[_],
  Collection[_],
  SizedWidget,
  PositionedWidget
] = Collection[Place[SizedWidget]] => Place[Collection[PositionedWidget]]

def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  Collection[_] : {Traverse, Zip},
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  FreeWidget,
  PositionedWidget
](
  isEventConsumed : Update[Boolean],
  updateContainerOrdered : TraverseChildrenOrdered[
    Update,
    Collection,
    FreeWidget,
    PositionedWidget
  ],
  positionedChildHandlesEvent : HandlesEvent[PositionedWidget, EnvironmentalEvent, Update[Option[FreeWidget]]],
  positionedMergesWithOldStates : MergesWithOldStates[PositionedWidget, RecompositionReaction, Option[FreeWidget]],
  positionedReactsOnRecomposition : ReactsOnRecomposition[PositionedWidget, RecompositionReaction],
  positionedHasInnerStates : HasInnerStates[PositionedWidget, RecompositionReaction],
  drawOrdered : Collection[PositionedWidget] => Draw,
  children: Collection[FreeWidget],
  layout: Collection[FreeWidget] => Place[Collection[PositionedWidget]],
  incrementalFreeChildrenFromPlaced: (
    PositionedWidget,
    Option[FreeWidget]
  ) => FreeWidget,
) : Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]] =
  layout(children).map(
    positionedChildren =>
      Widget.ValueWrapper(
        valueToDecorate = positionedChildren,
        valueAsFree = containerAsFree(
           children => layout(children.map(incrementalFreeChildrenFromPlaced(_, None)))
        ),
        valueIsDrawable = children => drawOrdered(children),
        valueHandlesEvent = containerHandlesEvent(
          childrenHandleEvent[
            Collection,
            Update,
            FreeWidget,
            PositionedWidget,
            EnvironmentalEvent,
          ](
            widgetHandlesEvent = positionedChildHandlesEvent,
            isEventConsumed = isEventConsumed,
            traverseContainerOrdered = updateContainerOrdered
          ),
          children =>
            layout(
              children.map(incrementalFreeChildrenFromPlaced(_, _))
            )
        ),
        valueMergesWithOldState = containerMergesWithOldStates(
          positionedMergesWithOldStates,
          children =>
            layout(
              children.map(incrementalFreeChildrenFromPlaced(_, _))
            )
        ),
        valueReactsOnRecomposition = containerReactsOnRecomposition(
          positionedReactsOnRecomposition
        ),
        valueHasInnerState = containerHasInnerStates(
          positionedHasInnerStates
        ),
      )
  )
end container


def container[
  Update[_] : Monad,
  Place[_] : Functor as PF,
  Collection[_] : {Traverse, Zip},
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  PositionedChild
](
  isEventConsumed : Update[Boolean],
  updateContainerOrdered : TraverseChildrenOrdered[
    Update,
    Collection,
    Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]],
    PositionedChild
  ],
  positionedChildHandlesEvent : HandlesEvent[PositionedChild, EnvironmentalEvent, Update[Option[Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]]]]],
  drawOrdered : Collection[PositionedChild] => Draw,
  children: Collection[Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]]],
  layout:
    Collection[
      Place[
        Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
      ]
    ] => Place[
      Collection[
        PositionedChild
      ]
    ],
  incrementalFreeChildrenFromPlaced: (
    PositionedChild,
      Option[Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]]]
    ) => Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]],
  unplaceWidget : PositionedChild => Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
) : Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]] =
  container(
    positionedChildHandlesEvent = positionedChildHandlesEvent,
    positionedMergesWithOldStates  = (positionedChild, pathToParent, oldStates) =>
      widgetMergesWithOldState(unplaceWidget(positionedChild), pathToParent, oldStates),
    positionedReactsOnRecomposition  = (positionedChild, pathToParent, states) =>
      widgetReactsOnRecomposition(unplaceWidget(positionedChild), pathToParent, states),
    positionedHasInnerStates  = positionedChild =>
      widgetHasInnerStates(unplaceWidget(positionedChild)),
    isEventConsumed = isEventConsumed,
    updateContainerOrdered = updateContainerOrdered,
    drawOrdered = drawOrdered,
    children = children,
    layout = layout,
    incrementalFreeChildrenFromPlaced = incrementalFreeChildrenFromPlaced,
  )
end container