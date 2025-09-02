package gui4s.desktop.kit.zio
package widgets.decorator

import effects.{*, given}
import widgets.DesktopWidget

import cats.syntax.all.*
import gui4s.core.geometry.Rect

import zio.*
import zio.interop.catz.*

import gui4s.desktop.kit.widgets.decorator.clip as genericClip

extension[Event](value : DesktopWidget[Event])
  def clip(path : Rect[Float] => Clip) : DesktopWidget[Event] =
    value.genericClip(path)
  end clip
end extension