package gui4s.android.kit.widgets

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.canvas.drawImage
import gui4s.desktop.widget.library.drawOnlyWidget

import org.jetbrains.skia.{Canvas, Image}

def image[
  IO[_] : Sync,
  Event
](
  image: Image,
): AndroidWidget[IO, Event] =
  drawOnlyWidget[
    UpdateC[IO, Event],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
  ](
    Sized(drawImage[ReaderT[IO, Canvas, *]](image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[OuterPlace[IO, *]],
    RecompositionReaction.empty[IO],
  )
end image
