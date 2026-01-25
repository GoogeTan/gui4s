package gui4s.android.kit.widgets.decorator

import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.core.geometry.Point3d
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.{PaddingWidget, Paddings}

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[IO[_] : Sync, Event] : PaddingWidget[AndroidWidget[IO, Event], Paddings[Float]] =
  gui4s.desktop.widget.library.decorator.gapPaddingWidget(
    container = containerWidget[IO, Id, Event](traverseOne),
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


extension[IO[_], Event](widget: AndroidWidget[IO, Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[Float])(using Sync[IO]): AndroidWidget[IO, Event] =
    gapPaddingWidget(paddings)(widget)
  end padding
end extension

