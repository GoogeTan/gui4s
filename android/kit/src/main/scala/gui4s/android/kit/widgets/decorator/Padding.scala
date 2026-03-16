package gui4s.android.kit.widgets.decorator

import cats.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.core.geometry.{InfinityOr, Point3d, Rect}
import gui4s.core.layout.{Measured, Sized}
import gui4s.core.widget.library.decorator.{PaddingWidget, Paddings}

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
      Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]],
      Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])],
      Rect[Float],
      Bounds,
      Point3d[Float]
    ](
      container = oneElementContainerWidget,
      boundsWithPaddings =
        PlacementEffect.withBoundsK(_.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))),
      innerPlaceWithPaddings = (child, bounds) =>
        Sized(
          new Point3d(paddings.topLeftCornerShift, 0f),
          child + paddings.addedBoundsRect,
        ).pure[PlacementEffect],
      makeMeta = (measured, point) => Measured((measured.value, point), measured.size, measured.bounds),
      sizeOfItem = _.size
    )
end gapPaddingWidget

def makeMeta[
  MeasurementUnit,
  BoundUnit,
  Event,
  Point
](
   measured : Measured[MeasurementUnit, BoundUnit, AndroidPlacedWidget[Event]],
   point : Point
) : (AndroidPlacedWidget[Event], Measured[MeasurementUnit, BoundUnit, Point]) =
  (measured.value, Measured(point, measured.size, measured.bounds))
end makeMeta

extension[Event](widget: AndroidWidget[Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[Float]): AndroidWidget[Event] =
    gapPaddingWidget[Event](paddings)(widget)
  end padding
end extension
