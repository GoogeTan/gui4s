package gui4s.core.widget.library.decorator

import gui4s.core.widget.Path

/**
 * Виджет, позволяющий обрабатывать внешние события.
 * Обработчик принимает нынешний путь, разположение виджета и событие.
 *
 * @tparam Widget
 * @tparam Update
 * @tparam WidgetPositionAndBounds
 * @tparam HandlableEvent
 */
type EventCatcherWithRect[Widget, Update, WidgetPositionAndBounds, HandlableEvent] =
  ((Path, WidgetPositionAndBounds, HandlableEvent) => Update) => Decorator[Widget]
