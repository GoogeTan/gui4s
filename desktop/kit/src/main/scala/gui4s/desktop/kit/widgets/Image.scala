package gui4s.desktop.kit
package widgets

import cats.*
import cats.data.ReaderT
import cats.effect.kernel.Sync
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.canvas.drawImage
import gui4s.desktop.widget.library.drawOnlyWidget
import gui4s.desktop.skija.{Canvas, Image}

def imageWidget[
  IO[_] : Sync,
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
    Sized(drawImage[ReaderT[IO, Canvas, *]](image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[OuterPlace[IO, *]],
    RecompositionReaction.empty[IO],
  )
end imageWidget
