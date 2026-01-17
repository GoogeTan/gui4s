package gui4s.desktop.kit
package widgets.decorator

import catnip.syntax.list.traverseOne
import cats.*
import cats.effect.kernel.Sync
import cats.syntax.all.*
import gui4s.core.geometry.Point3d
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.{Decorator, Paddings}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.OuterPlace.given
import gui4s.desktop.kit.widgets.*

def gapPaddingWidget[IO[_] : Sync, Event](paddings: Paddings[Float]): Decorator[DesktopWidget[IO, Event]] =
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
        OuterPlace.withBounds[IO, Sized[Float, DesktopPlacedWidget[IO, Event]]](
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

extension[IO[_], Event](widget: DesktopWidget[IO, Event])
  def padding(paddings: Paddings[Float])(using Sync[IO]): DesktopWidget[IO, Event] =
    gapPaddingWidget(paddings)(widget)
  end padding
end extension

