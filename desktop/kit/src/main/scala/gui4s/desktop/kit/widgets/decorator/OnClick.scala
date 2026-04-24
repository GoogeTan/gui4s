package gui4s.desktop.kit
package widgets.decorator

import cats.effect.*
import cats.syntax.all.*

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.RectAtPoint2d
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.clickCatcher as genericClickCatcher

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*

type ClickEventSource = DownEvent => Option[Point2d[Float]]

trait ClickCatcher:
  def apply[Event](eventOnClick: Event): Decorator[DesktopWidget[Event]]

  extension[Event](self: DesktopWidget[Event])
    def onClick(eventOnClick: Event) : DesktopWidget[Event] =
      apply(eventOnClick)(self)
    end onClick
  end extension
end ClickCatcher

def clickCatcher(
  extractEvent : ClickEventSource,
) : ClickCatcher =
  new ClickCatcher:
    override def apply[Event](eventOnClick: Event): Decorator[DesktopWidget[Event]] =
      genericClickCatcher[
        DesktopWidget[Event],
        UpdateC[Event],
        DownEvent,
        Point2d[Float],
        Situated[DesktopPlacedWidget[Event]]
      ](
        eventCatcherWithRect = eventCatcher,
        catchMouseEvent = callback =>
          Update.handleEnvironmentalEvents_(event =>
            extractEvent(event).fold(
              false.pure[UpdateC[Event]]
            )(callback)
          ),
        onClick = _ => Update.emitEvents[Event](List(eventOnClick)).as(true),
        isMouseIn = (shape, mousePosition) =>
          Update.getCornerCoordinates.map(
            coordinatesOfTopLeftCornet =>
              RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY)
                .containsPoint(mousePosition)
          )
      )
end clickCatcher

