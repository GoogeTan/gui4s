package gui4s.desktop.widget.library
package decorator

import catnip.syntax.monad.MonadErrorC
import cats.Id
import cats.~>

import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.PlacementStrategy
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Paddings

/**
 * Одноместный контейнер, добавляющий отступы вокруг виджета.
 */
def paddingWidget[
  Update[_],
  PlacementEffect[_] : MonadErrorC[PlaceError],
  Situated[_],
  Draw,
  RecompositionReaction,
  MeasuredWidget,
  PositionedWidget,
  MeasurementUnit : Fractional,
  PlaceError
](
  container : ContainerWidget[
    FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction],
    FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[MeasuredWidget],
      Rect[MeasurementUnit],
      Rect[InfinityOr[MeasurementUnit]],
      Id,
      PositionedWidget
    ]
  ],
  boundsWithPaddings : PlacementEffect ~> PlacementEffect,
  makeMeta : (MeasuredWidget, Point2d[MeasurementUnit]) => PositionedWidget,
  sizeOfItem : MeasuredWidget => Rect[MeasurementUnit],
  infinitePaddingInInfiniteContainer : => PlaceError,
  widget : FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction],
  paddings : Paddings[InfinityOr[MeasurementUnit]]
): FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction] =
  gui4s.core.widget.library.decorator.paddingWidget(
    container = container,
    infinitePaddingInInfiniteContainer = infinitePaddingInInfiniteContainer,
    boundsWithPaddings = boundsWithPaddings,
    sizeOfItem = sizeOfItem,
    makeMeta = makeMeta,
    widget = widget,
    paddings = paddings
  )
end paddingWidget
