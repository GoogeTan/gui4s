package gui4s.desktop.windowing.jwm

import cats.syntax.all.*
import io.github.humbleui.jwm.EventMouseScroll

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Axis.Horizontal
import gui4s.core.geometry.Rect

import gui4s.desktop.kit.effects.DownEvent
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.effects.Situated
import gui4s.desktop.kit.effects.Update
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.trueUpdateDecoratorWithRect

final case class InnerEvent(event : EventMouseScroll, childSize: Rect[Float])

def scrollEventSource: ScrollEventSource[EventMouseScroll] =
  new ScrollEventSource[EventMouseScroll]:
    override def emitLeftovers[Event](leftovers: EventMouseScroll): Update[Event, Unit] =
      Update.emitEnvironmentalEvents(List(DownEvent.UserEvent(leftovers)))
    end emitLeftovers

    def lower[Event](
      original: DesktopWidget[Event]
    ): DesktopWidget[Event] =
      trueUpdateDecoratorWithRect(
        original,
        situatedWidget =>
          Update.updateEnvironmentalEventsM_(events =>
            events.map {
              case DownEvent.UserEvent(event : EventMouseScroll) =>
                UserEvent(InnerEvent(event, situatedWidget.size))
              case event => event
            }.pure
          ) *> widgetHandlesEvent(situatedWidget.value)
      )
    
    override def eventSource[Event](
      original: ([Event2] => DesktopWidget[Event2] => DesktopWidget[Event2]) => DesktopWidget[Event],
      onScroll: (Situated[DesktopPlacedWidget[Event]], EventMouseScroll, Rect[Float]) => Event,
    ): DesktopWidget[Event] =
      trueUpdateDecoratorWithRect(
        original([Event2] => body => lower(body)),
        situatedWidget =>
          widgetHandlesEvent(situatedWidget.value)
            <* Update.updateEnvironmentalEventsM_(events =>
            val (scrollEvents, notScrollEvents) = events.partitionMap {
              case UserEvent(event: InnerEvent) =>
                Left((event.event, event.childSize))
              case event =>
                Right(event)
            }
            Update
              .emitEvents(scrollEvents.map(onScroll(situatedWidget, _, _)))
              .as(notScrollEvents)
          )
      )

    extension (event: EventMouseScroll)
      override def shiftAlong(axis: Axis): Float =
        if axis === Horizontal then
          event._deltaX
        else
          event._deltaY
        end if
      end shiftAlong
      
      override def withShiftAlong(axis: Axis, value: Float): EventMouseScroll =
        if axis === Horizontal then
          EventMouseScroll(
            value,
            event._deltaY,
            event._deltaChars,
            event._deltaLines,
            event._deltaPages,
            event._x,
            event._y,
            event._modifiers,
          )
        else
          EventMouseScroll(
            event._deltaX,
            value,
            event._deltaChars,
            event._deltaLines,
            event._deltaPages,
            event._x,
            event._y,
            event._modifiers,
          )
        end if
      end withShiftAlong
    end extension
end scrollEventSource

def scrollWidget: ScrollWidget =
  gui4s.desktop.kit.widgets.scrollWidget(
    scrollEventSource,
    animationWidget
  )
end scrollWidget
