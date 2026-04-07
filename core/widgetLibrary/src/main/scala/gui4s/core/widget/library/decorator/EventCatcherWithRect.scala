package gui4s.core.widget.library.decorator

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
  (WidgetPositionAndBounds => Update) => Decorator[Widget]
