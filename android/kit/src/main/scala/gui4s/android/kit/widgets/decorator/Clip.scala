package gui4s.android.kit.widgets.decorator

import gui4s.core.geometry.Rect
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Clip.given
import gui4s.android.kit.widgets.AndroidWidget

def clipWidget[Event](value : AndroidWidget[Event], path : Rect[Float] => Clip) : AndroidWidget[Event] =
  gui4s.desktop.widget.library.decorator.clipWidget[
    UpdateC[Event],
    PlacementEffect,
    Situated,
    Draw,
    RecompositionReaction,
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

extension[Event](value : AndroidWidget[Event])
  def clip(path : Rect[Float] => Clip) : AndroidWidget[Event] =
    clipWidget[Event](value, path)
  end clip
end extension
