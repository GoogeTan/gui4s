package gui4s.desktop.kit.cats
package widgets.decorator

import effects.Clip.given
import effects.OuterPlace.given
import effects.Update.given
import effects.given
import widgets.DesktopWidget

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import gui4s.core.geometry.{Point3d, Rect}
import gui4s.desktop.kit.common.widgets.decorator.gapPadding as generalGapPadding
import gui4s.desktop.skija.drawAt
import gui4s.desktop.widget.library.decorator.{Paddings, gapPaddingWidget}

extension[Event](value : DesktopWidget[Event])
  def gapPadding(paddings: Paddings[Float]): DesktopWidget[Event] =
    value.generalGapPadding(paddings)
  end gapPadding
end extension
