package gui4s.desktop.kit
package common.widgets.decorator

import catnip.ForeignFunctionInterface
import catnip.syntax.functionk.given 
import cats.Monad
import gui4s.core.geometry.Point3d
import gui4s.desktop.kit.common.widgets.DesktopWidget
import gui4s.desktop.kit.common.effects.*
import gui4s.desktop.skija.drawAt
import gui4s.desktop.widget.library.decorator.{Paddings, gapPaddingWidget}

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
      paddings =>
        asFunctionK(
          [T] => place =>
            OuterPlace.withBounds(place, _.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_)))
        ),
      paddings => update => (path, event) =>
        Update.withCornerCoordinates(update(path, event), _ + new Point3d(paddings.topLeftCornerShift)),
      paddings => draw => drawAt(ffi, draw.value, paddings.left, paddings.top),
    )(paddings)(value)
  end gapPadding
end extension
