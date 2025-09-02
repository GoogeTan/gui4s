package gui4s.desktop.kit
package common.widgets

import common.effects.*
import common.effects.Place.given

import catnip.ForeignFunctionInterface
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.desktop.skija.drawImage
import gui4s.desktop.widget.library.drawOnlyWidget
import io.github.humbleui.skija.Image

def image[
  IO[_] : {Monad, ForeignFunctionInterface as ffi},
  Event
](
  image: Image,
): DesktopWidget[IO, Event] =
  drawOnlyWidget[
    UpdateC[IO, Event],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
  ](
    Sized(drawImage(ffi, image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[OuterPlace[IO, *]],
    RecompositionReaction.empty[IO],
  )
end image
