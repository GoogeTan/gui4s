package gui4s.desktop.kit
package widgets

import cats._
import cats.effect._
import cats.syntax.all._

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.skija.Image
import gui4s.desktop.skija.canvas.drawImage
import gui4s.desktop.widget.library.drawOnlyWidget

def imageWidget[
  Event
](
  image: Image,
): DesktopWidget[Event] =
  drawOnlyWidget[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
  ](
    Sized(drawImage[DrawM](image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[PlacementEffect],
    RecompositionReaction.empty[IO],
  )
end imageWidget
