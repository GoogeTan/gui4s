package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Clip.given
import effects.OuterPlace.given
import effects.Update.given

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.desktop.widget.library.decorator.clipWidget
import widgets.DesktopWidget

extension[IO[_] : {Monad, ForeignFunctionInterface as ffi}, Event](value : DesktopWidget[IO, Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[IO, Event] =
    clipWidget[
      UpdateC[IO, Event],
      OuterPlace[IO, *],
      InnerPlace,
      Draw[IO],
      RecompositionReaction[IO],
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
  end clip
end extension