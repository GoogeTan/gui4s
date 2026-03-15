package gui4s.desktop.kit
package widgets.decorator

import cats.effect._
import cats.syntax.all._

import gui4s.core.geometry.Rect

import gui4s.desktop.kit.effects.Clip.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget

def clipWidget[Event](value : DesktopWidget[Event], path : Rect[Float] => Clip) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.decorator.clipWidget[
    UpdateC[Event],
    PlacementEffect,
    Situated,
    Draw,
    RecompositionReaction,
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

extension[Event](value : DesktopWidget[Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[Event] =
    clipWidget[Event](value, path)
  end clip
end extension