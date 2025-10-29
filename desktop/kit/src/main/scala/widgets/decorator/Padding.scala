package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Draw.given
import widgets.DesktopWidget

import catnip.syntax.functionk.given
import cats.effect.kernel.Sync
import gui4s.core.geometry.Point3d
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.decorator.{Paddings, gapPaddingWidget}

def gapPadding[IO[_] : Sync, Event](value : DesktopWidget[IO, Event])(paddings: Paddings[Float]): DesktopWidget[IO, Event] =
  gapPaddingWidget[
    UpdateC[IO, Event],
    OuterPlace[IO, *],
    InnerPlace,
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    Paddings[Float],
  ](
    paddings =>
      asFunctionK(
        [T] => place =>
          OuterPlace.withBounds(place, _.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_)))
      ),
    paddings => update => (path, event) =>
      Update.withCornerCoordinates(update(path, event), _ + new Point3d(paddings.topLeftCornerShift)),
    paddings => draw => drawAt(paddings.left, paddings.top, draw.value),
  )(paddings)(value)
end gapPadding
