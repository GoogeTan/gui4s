package gui4s.desktop.kit
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
import gui4s.desktop.widget.library.drawOnlyWidget
import gui4s.desktop.skija.drawImage
import io.github.humbleui.skija.Image
import catnip.ForeignFunctionInterface
import cats.*
import cats.syntax.all.*
import effects.OuterPlace.given

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
