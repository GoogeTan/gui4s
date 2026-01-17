package gui4s.desktop.kit
package widgets.decorator

import cats.effect.Sync
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Clip.given
import gui4s.desktop.kit.widgets.DesktopWidget

import scala.annotation.targetName

def clipWidget[IO[_] : Sync, Event](value : DesktopWidget[IO, Event], path : Rect[Float] => Clip) : DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.decorator.clipWidget[
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
end clipWidget

extension[IO[_], Event](value : DesktopWidget[IO, Event])
  def clip(path : Rect[Float] => Clip)(using Sync[IO]) : DesktopWidget[IO, Event] =
    clipWidget[IO, Event](value, path)
  end clip
end extension