package gui4s.desktop.kit
package widgets.decorator

import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.{Measured, Sized}
import gui4s.core.widget.library.decorator.PaddingWidget
import gui4s.core.widget.library.decorator.Paddings
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[Event] : PaddingWidget[DesktopWidget[Event], Paddings[Float]] =
  paddings =>
    gui4s.desktop.widget.library.decorator.gapPaddingWidget[
      UpdateC[Event],
      PlacementEffect,
      Situated,
      Draw,
      RecompositionReaction,
      DownEvent,
      Measured[Rect[Float], Bounds, DesktopPlacedWidget[Event]],
      Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])],
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

extension[Event](widget: DesktopWidget[Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[Float]): DesktopWidget[Event] =
    gapPaddingWidget(paddings)(widget)
  end padding
end extension

