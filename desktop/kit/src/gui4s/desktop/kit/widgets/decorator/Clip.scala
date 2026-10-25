package gui4s.desktop.kit
package widgets.decorator

import scala.annotation.targetName

import cats.effect.*
import cats.syntax.all.*

import gui4s.core.layout.Sized

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Clip.given
import gui4s.desktop.kit.widgets.DesktopWidget

def clipWidget[Event](value : DesktopWidget[Event], shape : ShapeInBounds) : DesktopWidget[Event] =
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
    place => shape(place.size),
  )(value)
end clipWidget

extension[Event](value : DesktopWidget[Event])
  def clip(shape : Shape) : DesktopWidget[Event] =
    clipWidget[Event](value, rect => shape(rect).pure[PlacementEffect])
  end clip

  @targetName("clipF")
  def clip(shape: ShapeInBounds): DesktopWidget[Event] =
    clipWidget[Event](value, shape)
  end clip
end extension