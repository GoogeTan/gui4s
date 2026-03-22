package gui4s.core.widget.library.decorator

import gui4s.core.widget.Path

/**
 * Виджет, позволяющий обрабатывать внешние события.
 * Обработчик принимает нынешний путь и расположение виджета.
 *
 * @tparam Widget
 * @tparam Update
 * @tparam WidgetPositionAndBounds
 * @tparam HandlableEvent
 */
type EventCatcherWithRect[Widget, Update, WidgetPositionAndBounds] =
  ((WidgetPositionAndBounds, Path) => Update) => Decorator[Widget]
