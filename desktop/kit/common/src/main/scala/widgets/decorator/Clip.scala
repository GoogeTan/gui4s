package gui4s.desktop.kit
package common.widgets.decorator

import common.effects.*
import common.effects.Clip.given 
import common.widgets.DesktopWidget

import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.desktop.widget.library.decorator.clipWidget
import cats.effect.Sync

def clip[IO[_] : Sync, Event](value : DesktopWidget[IO, Event])(path : Rect[Float] => Clip) : DesktopWidget[IO, Event] =
  clipWidget[
    UpdateC[IO, Event],
    OuterPlace[IO, *],
    InnerPlace,
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    Clip
  ](
    [T] => (shape, update) =>
      Update.withClip(
        update,
        (oldShape, point) =>
          oldShape |+| Clip.moveClipToPoint(shape, point)
      ),
    Clip.drawClipped,
    place => path(place.size),
  )(value)
end clip