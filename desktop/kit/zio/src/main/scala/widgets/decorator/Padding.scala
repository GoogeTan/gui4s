package gui4s.desktop.kit.zio
package widgets.decorator

import widgets.DesktopWidget

import gui4s.desktop.kit.common.widgets.decorator.gapPadding as generalGapPadding
import gui4s.desktop.widget.library.decorator.Paddings
import zio.interop.catz.*

extension[Event](value : DesktopWidget[Event])
  def gapPadding(paddings: Paddings[Float]): DesktopWidget[Event] =
    value.generalGapPadding(paddings)
  end gapPadding
end extension
