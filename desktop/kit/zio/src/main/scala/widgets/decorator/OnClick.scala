package gui4s.desktop.kit.zio
package widgets.decorator

import effects.given
import widgets.DesktopWidget

import cats.syntax.all.*
import gui4s.core.geometry.Point2d
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.Decorator
import zio.*
import zio.interop.catz.*

def clickCatcher[Event](mousePosition : Task[Point2d[Float]], eventOnClick : Event) : Decorator[DesktopWidget[Event]] =
  gui4s.desktop.kit.common.widgets.decorator.clickCatcher(mousePosition, eventOnClick)
end clickCatcher