package gui4s.desktop.widget.library
package decorator

import cats.{FlatMap, Functor, Id, ~>}
import gui4s.core.layout.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[
  Update[_] : Functor,
  PlacementEffect[_] : FlatMap,
  Situated[_],
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
  MeasuredWidget,
  PositionedWidget,
  Size,
  Bounds,
  Point,
](
  container : ContainerWidget[
    FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
    FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[MeasuredWidget],
      Size,
      Bounds,
      Id,
      PositionedWidget
    ]
  ],
  boundsWithPaddings : PlacementEffect ~> PlacementEffect,
  innerPlaceWithPaddings : OneElementPlacementStrategy[PlacementEffect, Size, Size, Bounds, Point],
  makeMeta : (
    MeasuredWidget,
    Point
  ) => PositionedWidget,
  sizeOfItem : MeasuredWidget => Size
): Decorator[
  FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent]
] =
  gui4s.core.widget.library.decorator.gapPaddingWidget(
    container = container,
    boundsWithPaddings = boundsWithPaddings,
    innerPlaceWithPaddings = innerPlaceWithPaddings,
    makeMeta = makeMeta,
    sizeOfItem = sizeOfItem
  )
end gapPaddingWidget
