package gui4s.android.kit.widgets

import cats.effect.IO
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.canvas.drawImage
import gui4s.desktop.widget.library.drawOnlyWidget
import cats.data.ReaderT

import org.jetbrains.skia.{Canvas, Image}

def image[
  Event
](
  image: Image,
): AndroidWidget[Event] =
  drawOnlyWidget[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
  ](
    Sized(drawImage[ReaderT[IO, Canvas, *]](image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[PlacementEffect],
    RecompositionReaction.empty[IO],
  )
end image
