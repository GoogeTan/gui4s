package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import effects.Clip.given
import effects.OuterPlace.given
import effects.Update.given

import catnip.effect.SyncForeignFunctionInterface
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.decktop.widget.library.decorator.clipWidget
import gui4s.desktop.kit.zio.widgets.DesktopWidget

extension[Event](value : DesktopWidget[Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[Event] =
    clipWidget[
      UpdateC[Event],
      OuterPlace,
      InnerPlace,
      Draw,
      RecompositionReaction,
      DownEvent,
      Clip
    ](
      [T] => (shape, update) =>
        Update.withClip(
          update,
          (oldShape, point) =>
            oldShape |+| Clip.moveClipToPoint(shape, point)
        ),
      Draw.drawClipped,
      place => path(place.size),
    )(value)
  end clip
end extension
