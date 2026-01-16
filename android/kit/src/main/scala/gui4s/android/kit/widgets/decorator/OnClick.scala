package gui4s.android.kit.widgets.decorator

import gui4s.core.geometry.{Point2d, RectAtPoint2d}
import gui4s.core.widget.library.decorator.{Decorator, clickCatcher as genericClickCatcher}
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

def clickCatcher[IO[_] : Monad, Event, MouseEvent](
                                        mousePosition : IO[Point2d[Float]],
                                        eventOnClick : Event,
                                        extractEvent : DownEvent => Option[MouseEvent]
                                      ) : Decorator[AndroidWidget[IO, Event]] =
  genericClickCatcher(
    eventCatcherWithRect = eventCatcher,
    currentMousePosition = Update.liftK[IO, Event](mousePosition),
    appropriateEvent = extractEvent,
    onClick = {
      (_, _) => Update.emitEvents(List(eventOnClick)).as(true)
    },
    isIn = point => shape =>
      Update.getCornerCoordinates.map(
        coordinatesOfTopLeftCornet =>
          RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
      )
  )
end clickCatcher