package gui4s.desktop.kit.cats
package widgets.decorator

import widgets.DesktopWidget

import cats.effect.IO
import gui4s.core.geometry.Point2d
import gui4s.desktop.widget.library.decorator.Decorator

def clickCatcher[Event](mousePosition : IO[Point2d[Float]], eventOnClick : Event) : Decorator[DesktopWidget[Event]] =
  gui4s.desktop.kit.common.widgets.decorator.clickCatcher(mousePosition, eventOnClick)
end clickCatcher