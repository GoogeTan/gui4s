package gui4s.android.kit.widgets.decorator

import gui4s.core.geometry.{Point2d, RectAtPoint2d}
import gui4s.core.widget.library.decorator.{Decorator, clickCatcher as genericClickCatcher}
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*

trait ClickCatcher:
  def apply[Event](eventOnClick: Event): Decorator[AndroidWidget[Event]]

  extension[Event](self: AndroidWidget[Event])
    def onClick(eventOnClick: Event) : AndroidWidget[Event] =
      apply(eventOnClick)(self)
    end onClick
  end extension
end ClickCatcher

def clickCatcher(
                  queue: Queue[IO, DownEvent],
                  extractEvent : DownEvent => Option[Point2d[Float]]
                ) : ClickCatcher =
  new ClickCatcher:
    override def apply[Event](eventOnClick: Event): Decorator[AndroidWidget[Event]] =
      genericClickCatcher[
        AndroidWidget[Event],
        UpdateC[Event],
        DownEvent,
        Point2d[Float],
        Situated[AndroidPlacedWidget[Event]]
      ](
        eventCatcherWithRect = eventCatcher,
        catchMouseEvent = callback =>
          Update.handleEnvironmentalEvents_(event =>
            extractEvent(event).fold(
              false.pure[UpdateC[Event]]
            )(callback)
          ),
        onClick = _ =>
          Update.emitEvents[Event](List(eventOnClick)).as(true),
        isMouseIn = (shape, mousePosition) =>
          Update.getCornerCoordinates.map(
            coordinatesOfTopLeftCornet =>
              RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY)
                .containsPoint(mousePosition)
          )
      )
end clickCatcher