package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.OuterPlace.given
import effects.Update.given
import effects.Place.given
import effects.RecompositionReaction.given

import cats.Monoid
import cats.effect.IO
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.decktop.widget.library.drawOnlyWidget
import gui4s.desktop.skija.drawImage
import io.github.humbleui.skija.Image
import catnip.effect.SyncForeignFunctionInterface
import cats.syntax.all.*

def image[Event](
                  image: Image,
                ): DesktopWidget[Event] =
  drawOnlyWidget[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
  ](
    Sized(Draw.drawImage(image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[OuterPlace],
    Monoid[RecompositionReaction].empty,
  )
end image
