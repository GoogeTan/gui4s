package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Clip.given
import effects.OuterPlace.given
import effects.Update.given
import widgets.DesktopWidget

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import gui4s.core.geometry.{Point3d, Rect}
import gui4s.desktop.widget.library.decorator.{Paddings, gapPaddingWidget}
import gui4s.desktop.skija.drawAt

extension[IO[_] : {Monad, ForeignFunctionInterface as ffi}, Event](value : DesktopWidget[IO, Event])
  def gapPadding(paddings: Paddings[Float]): DesktopWidget[IO, Event] =
    gapPaddingWidget[
      UpdateC[IO, Event],
      OuterPlace[IO, *],
      InnerPlace,
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
      Paddings[Float],
    ](
      paddings => [T] => place =>
        OuterPlace.withBounds(place, _.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))),
      paddings => update => (path, event) =>
        Update.withCornerCoordinates(update(path, event), _ + new Point3d(paddings.topLeftCornerShift)),
      paddings => draw => drawAt(ffi, draw.value, paddings.left, paddings.top),
    )(paddings)(value)
  end gapPadding
end extension
