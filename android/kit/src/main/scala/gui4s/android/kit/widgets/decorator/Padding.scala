package gui4s.android.kit.widgets.decorator

import gui4s.core.geometry.Point3d
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.{Decorator, Paddings}
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*

def gapPaddingWidget[IO[_] : Sync, Event](paddings: Paddings[Float]): Decorator[AndroidWidget[IO, Event]] =
  original =>
    containerWidget[
      IO,
      Id,
      Event,
    ](
      traverseOne
    )(
      original,
      child =>
        OuterPlace.withBounds[IO, Sized[Float, AndroidPlacedWidget[IO, Event]]](
          child,
          originalBounds => originalBounds.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))
        ).map(sizedChild =>
            Sized(
              (
                sizedChild.value,
                Point3d(paddings.left, paddings.top, 0f)
              ),
              sizedChild.size + paddings.addedBoundsRect
            )
        )
    )
end gapPaddingWidget

extension[IO[_], Event](widget: AndroidWidget[IO, Event])
  def padding(paddings: Paddings[Float])(using Sync[IO]): AndroidWidget[IO, Event] =
    gapPaddingWidget(paddings)(widget)
  end padding
end extension

