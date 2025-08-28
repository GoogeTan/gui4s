package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import effects.Update.given
import widgets.DesktopWidget

import cats.syntax.all.*
import gui4s.core.geometry.{Point2d, RectAtPoint2d}
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library.decorator.{Decorator, clickCatcher as genericClickCatcher}
import zio.*
import zio.interop.catz.*

def clickCatcher[Event](mousePosition : Task[Point2d[Float]], eventOnClick : Event) : Decorator[DesktopWidget[Event]] =
  genericClickCatcher(
    eventCatcherWithRect = eventCatcher,
    currentMousePosition = Update.liftK[Event](mousePosition),
    approprieteEvent = DownEvent.extractMouseClickEvent,
    onClick = (_, _) => Update.emitEvents(List(eventOnClick)).as(true),
    isIn = point => shape =>
      Update.getCornerCoordinates[List[Event]].map(
        coordinatesOfTopLeftCornet =>
          RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
      )
  )
end clickCatcher
