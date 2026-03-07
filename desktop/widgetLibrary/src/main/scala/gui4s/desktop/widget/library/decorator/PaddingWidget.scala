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
  Meta,
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
      (WidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent], Meta)
    ]
  ],
  boundsWithPaddings : PlacementEffect ~> PlacementEffect,
  innerPlaceWithPaddings : OneElementPlacementStrategy[PlacementEffect, Size, Size, Bounds, Point],
  makeMeta : (
    MeasuredWidget,
    Point
  ) => (
    WidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
    Meta
  ),
  sizeOfItem : MeasuredWidget => Size
): Decorator[
  FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent]
] =
  gui4s.core.widget.library.decorator.gapPaddingWidget[
    FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
    WidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
    PlacementEffect,
    MeasuredWidget,
    Meta,
    Size,
    Bounds,
    Point,
  ](
    container = container,
    boundsWithPaddings = boundsWithPaddings,
    innerPlaceWithPaddings = innerPlaceWithPaddings,
    makeMeta = makeMeta,
    sizeOfItem = sizeOfItem
  )
end gapPaddingWidget
