package gui4s.android.kit.widgets.decorator

import gui4s.core.geometry.Rect
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Clip.given
import gui4s.android.kit.widgets.AndroidWidget

import scala.annotation.targetName

def clipWidget[IO[_] : Sync, Event](value : AndroidWidget[IO, Event], path : Rect[Float] => Clip) : AndroidWidget[IO, Event] =
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

extension[IO[_], Event](value : AndroidWidget[IO, Event])
  def clip(path : Rect[Float] => Clip)(using Sync[IO]) : AndroidWidget[IO, Event] =
    clipWidget[IO, Event](value, path)
  end clip
end extension
