package gui4s.desktop.kit.cats
package widgets.decorator

import effects.{*, given}
import widgets.DesktopWidget

import gui4s.core.geometry.Rect
import gui4s.desktop.kit.common.widgets.decorator.clip as genericClip

extension[Event](value : DesktopWidget[Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[Event] =
    value.genericClip(path)
  end clip
end extension