package gui4s.android.kit.widgets.decorator

import cats.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.core.geometry.{InfinityOr, Point3d, Rect}
import gui4s.core.layout.{ElementPlacementResult, Measured}
import gui4s.core.widget.library.decorator.{PaddingWidget, Paddings}
import gui4s.core.widget.library.LayersMetadata

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[Event] : PaddingWidget[AndroidWidget[Event], Paddings[Float]] =
  paddings =>
    gui4s.desktop.widget.library.decorator.gapPaddingWidget[
      UpdateC[Event],
      PlacementEffect,
      Situated,
      Draw,
      RecompositionReaction,
      DownEvent,
      Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]],
      LayersMetadata[Point3d[Float], Rect[Float], Bounds],
      Rect[Float],
      Bounds,
      Point3d[Float]
    ](
      container = oneElementContainerWidget,
      boundsWithPaddings =
        PlacementEffect.withBoundsK(_.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))),
      innerPlaceWithPaddings = (child, bounds) =>
        ElementPlacementResult[Id, Rect[Float], Point3d[Float]](
          child + paddings.addedBoundsRect,
          new Point3d(paddings.topLeftCornerShift, 0f)
        ).pure[PlacementEffect],
      makeMeta = makeMeta[Float, InfinityOr[Float], Event, Point3d[Float]],
      sizeOfItem = _.size
    )
end gapPaddingWidget

def makeMeta[
  MeasurementUnit,
  Bounds,
  Event,
  Point
](
   measured : Measured[MeasurementUnit, Bounds, AndroidPlacedWidget[Event]],
   point : Point
) : (AndroidPlacedWidget[Event], LayersMetadata[Point, Rect[MeasurementUnit], Rect[Bounds]]) =
  (measured.value, LayersMetadata(point, measured.size, measured.bounds))
end makeMeta

extension[Event](widget: AndroidWidget[Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[Float]): AndroidWidget[Event] =
    gapPaddingWidget[Event](paddings)(widget)
  end padding
end extension
