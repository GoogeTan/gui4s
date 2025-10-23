package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import widgets.DesktopWidget

import gui4s.core.geometry.Rect

import zio.*
import zio.interop.catz.*

extension[Event](value : DesktopWidget[Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[Event] =
    gui4s.desktop.kit.common.widgets.decorator.clip(value)(path)
  end clip
end extension