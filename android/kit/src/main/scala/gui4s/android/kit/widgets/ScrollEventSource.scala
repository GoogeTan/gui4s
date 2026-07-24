package gui4s.android.kit.widgets

import gui4s.core.geometry.*

import gui4s.android.kit.effects.*

final case class ScrollEvent(dx: Float, dy: Float)

/**
 * Этот трейт предоставляет набор методов для работы с внешними событиями прокрутки.
 */
trait ScrollEventSource[ScrollEvent]:
  extension (event : ScrollEvent)
    def shiftAlong(axis : Axis) : Float
    def withShiftAlong(axis: Axis, value : Float) : ScrollEvent
  end extension

  /**
   * Должен быть вызван в виджете состояния, чтобы вернуть не поглощенную прокрутку
   */
  def emitLeftovers[Event](leftovers: ScrollEvent): Update[Event, Unit]

  /**
   * Должен использоваться уже снаружи виджета прокрутки для получения прокрутки в виде события, которое может обработать виджет состояния
   *
   * @param original Получает параметром декоратор, ловящий события скролла и добавляющий к ним информацию о размере
   */
  def eventSource[Event](
    original: ([Event2] => AndroidWidget[Event2] => AndroidWidget[Event2]) => AndroidWidget[Event],
    onScroll: (Situated[AndroidPlacedWidget[Event]], ScrollEvent, Rect[Float]) => Event,
  ): AndroidWidget[Event]
end ScrollEventSource
