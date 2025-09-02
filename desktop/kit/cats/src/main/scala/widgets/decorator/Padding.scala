package gui4s.desktop.kit.cats
package widgets.decorator

import effects.given
import widgets.DesktopWidget

import cats.effect.IO
import gui4s.desktop.kit.common.widgets.decorator.gapPadding as generalGapPadding
import gui4s.desktop.widget.library.decorator.Paddings

extension[Event](value : DesktopWidget[Event])
  def gapPadding(paddings: Paddings[Float]): DesktopWidget[Event] =
    value.generalGapPadding(paddings)
  end gapPadding
end extension
