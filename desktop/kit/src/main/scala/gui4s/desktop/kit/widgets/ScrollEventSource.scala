package gui4s.desktop.kit.widgets

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.widget.library.decorator.trueUpdateDecoratorWithRect
import gui4s.desktop.widget.library.widgetHandlesEvent

final case class ScrollEvent(dx: Float, dy: Float)

/**
 * Этот трейт предоставляет набор методов для работы с внешними событиями прокрутки.
 *
 * На практике требуется не 1 метод, который ловит внешнее событие скролла, а два.
 * Это связано с тем, что виджету прокрутки необходимо знать и размеры дочернего виджета, и собственные размеры.
 * Так как каждый виджет знает только свои размеры, приходится делать два виджета для того, чтобы получить и те, и те размеры.
 *
 * @todo возможно, стоит разрешить виджету состояния знать свой размер при обработке событий. Тогда этот интерфейс можно будет значительно упростить
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
    original: ([Event2] => DesktopWidget[Event2] => DesktopWidget[Event2]) => DesktopWidget[Event],
    onScroll: (Situated[DesktopPlacedWidget[Event]], ScrollEvent, Rect[Float]) => Event,
  ): DesktopWidget[Event]
end ScrollEventSource
