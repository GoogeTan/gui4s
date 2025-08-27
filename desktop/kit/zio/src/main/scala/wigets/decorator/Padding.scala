package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import effects.Clip.given
import effects.OuterPlace.given
import effects.Update.given
import widgets.DesktopWidget

import catnip.effect.SyncForeignFunctionInterface
import cats.effect.IO
import cats.syntax.all.*
import gui4s.core.geometry.{Point3d, Rect}
import gui4s.decktop.widget.library.decorator.{Paddings, gapPaddingWidget}
import gui4s.desktop.skija.drawAt

extension [Event](value : DesktopWidget[Event])
  def gapPadding(paddings: Paddings[Float]): DesktopWidget[Event] =
    gapPaddingWidget[
      UpdateC[Event],
      OuterPlace,
      InnerPlace,
      Draw,
      RecompositionReaction,
      DownEvent,
      Paddings[Float],
    ](
      paddings => [T] => place =>
        OuterPlace.withBounds(place, _.cut(paddings.horizontalLength, paddings.verticalLength, _.minus(_))),
      paddings => update => (path, event) =>
        Update.withCornerCoordinates(update(path, event), _ + new Point3d(paddings.topLeftCornerShift)),
      paddings => draw => drawAt(SyncForeignFunctionInterface[IO](), draw.value, paddings.left, paddings.top),
    )(paddings)(value)
  end gapPadding
end extension
