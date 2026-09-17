package gui4s.desktop.windowing.jwm

import io.github.humbleui.jwm.EventMouseButton

import gui4s.core.geometry.Point2d

import gui4s.desktop.kit.effects.DownEvent
import gui4s.desktop.kit.widgets.decorator.ClickEventSource

def jwmClickEventSource: ClickEventSource =
  case DownEvent.UserEvent(mouseEvent : EventMouseButton) if mouseEvent.isPressed =>
    Some(Point2d(mouseEvent._x.toFloat, mouseEvent._y.toFloat))
  case _ =>
    None
end jwmClickEventSource



