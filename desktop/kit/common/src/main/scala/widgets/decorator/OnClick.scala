package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Update.given
import widgets.DesktopWidget

import cats.Monad
import cats.syntax.all.*
import gui4s.core.geometry.{Point2d, RectAtPoint2d}
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.{Decorator, clickCatcher as genericClickCatcher}

def clickCatcher[IO[_] : Monad, Event](mousePosition : IO[Point2d[Float]], eventOnClick : Event) : Decorator[DesktopWidget[IO, Event]] =
  genericClickCatcher(
    eventCatcherWithRect = eventCatcher,
    currentMousePosition = Update.liftK[IO, Event](mousePosition),
    approprieteEvent = DownEvent.extractMouseClickEvent,
    onClick = (_, _) => Update.emitEvents(List(eventOnClick)).as(true),
    isIn = point => shape =>
      Update.getCornerCoordinates.map(
        coordinatesOfTopLeftCornet =>
          RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
      )
  )
end clickCatcher