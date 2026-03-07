package gui4s.desktop.kit
package widgets.decorator

import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.{Point3d, Rect, InfinityOr}
import gui4s.core.layout.{ElementPlacementResult, Measured}
import gui4s.core.widget.library.LayersMetadata
import gui4s.core.widget.library.decorator.{PaddingWidget, Paddings}
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
      Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]],
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

def makeMeta[M, B, T, P](measured : Measured[M, B, DesktopPlacedWidget[T]], point : P) : (DesktopPlacedWidget[T], LayersMetadata[P, Rect[M], Rect[B]]) =
  (measured.value, LayersMetadata(point, measured.size, measured.bounds))
end makeMeta

extension[Event](widget: DesktopWidget[Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[Float]): DesktopWidget[Event] =
    gapPaddingWidget(paddings)(widget)
  end padding
end extension

