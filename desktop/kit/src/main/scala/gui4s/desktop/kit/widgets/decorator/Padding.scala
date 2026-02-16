package gui4s.desktop.kit
package widgets.decorator

import catnip.syntax.list.traverseOne
import cats._
import cats.arrow.FunctionK
import cats.effect.*

import gui4s.core.geometry.Point3d
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.PaddingWidget
import gui4s.core.widget.library.decorator.Paddings

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[Event] : PaddingWidget[DesktopWidget[Event], Paddings[Float]] =
  gui4s.desktop.widget.library.decorator.gapPaddingWidget(
    container = containerWidget[Id, Event](traverseOne),
    boundsWithPaddings = paddings => FunctionK.lift([T] => effect =>
      PlacementEffect.withBounds[IO, T](
        effect,
        originalBounds => originalBounds.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))
      )
    ),
    innerPlaceWithPaddings =
      [T] => paddings => sizedChild =>
        Sized(
          (
            sizedChild.value,
            Point3d(paddings.left, paddings.top, 0f)
          ),
          sizedChild.size + paddings.addedBoundsRect
        )
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

