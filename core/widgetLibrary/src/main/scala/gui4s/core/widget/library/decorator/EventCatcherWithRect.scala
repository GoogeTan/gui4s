package gui4s.core.widget.library.decorator

import gui4s.core.widget.Path

type EventCatcherWithRect[Widget, Update, Rect, HandlableEvent] =
  ((Path, Rect, HandlableEvent) => Update) => Decorator[Widget]
