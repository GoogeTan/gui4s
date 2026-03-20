package gui4s.desktop.kit.widgets

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.pure.PureInput
import gui4s.core.geometry.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.widget.library.decorator.trueUpdateDecoratorWithRect
import gui4s.desktop.widget.library.widgetHandlesEvent

final case class ScrollEvent(dx: Float, dy: Float)

final case class InnerEvent(event : ScrollEvent, childSize: Rect[Float])

/**
 * Этот трейт предоставляет набор методов для работы с внешними событиями прокрутки.
 *
 * На практике требуется не 1 метод, который ловит внешнее событие скролла, а два.
 * Это связано с тем, что виджету прокрутки необходимо знать и размеры дочернего виджета, и собственные размеры.
 * Так как каждый виджет знает только свои размеры, приходится делать два виджета для того, чтобы получить и те, и те размеры.
 *
 * @todo возможно, стоит разрешить виджету состояния знать свой размер при обработке событий. Тогда этот интерфейс можно будет значительно упростить
 */
trait ScrollEventSource:
  /**
   * Должен быть вызван в виджете состояния, чтобы вернуть не поглощенную прокрутку
   */
  def emitLeftovers[Event](leftovers: Point2d[Float]): Update[Event, Unit]

  /**
   * Должен использоваться уже снаружи виджета прокрутки для получения прокрутки в виде события, которое может обработать виджет состояния
   *
   * @param original Получает параметром декоратор, ловящий события скролла и добавляющий к ним информацию о размере
   */
  def eventSource[Event](
    original: ([Event2] => DesktopWidget[Event2] => DesktopWidget[Event2]) => DesktopWidget[Event],
    onScroll: (Situated[DesktopPlacedWidget[Event]], Point2d[Float], Rect[Float]) => Event,
  ): DesktopWidget[Event]
end ScrollEventSource

def scrollEventSource[
  Window,
  Cursor,
  Joystick
](
  glfw: PureInput[IO, IO[Unit], Window, Cursor, Joystick],
  window: Window,
  eventBus: Queue[IO, DownEvent]
): Init[ScrollEventSource] =
  Init.eval(
    glfw.addScrollCallback(
      window,
      (_, dx, dy) =>
        eventBus.offer(UserEvent(ScrollEvent(dx.toFloat, dy.toFloat)))
    )
  ).as(
    new ScrollEventSource:
      override def emitLeftovers[Event](leftovers: Point2d[Float]): Update[Event, Unit] =
        Update.emitEnvironmentalEvents(List(UserEvent(ScrollEvent(leftovers.x, leftovers.y))))
      end emitLeftovers

      def lower[Event](
        original: DesktopWidget[Event]
      ): DesktopWidget[Event] =
        trueUpdateDecoratorWithRect(
          original,
          situatedWidget =>
            Update.updateEnvironmentalEventsM_(events =>
              events.map {
                case UserEvent(event: ScrollEvent) =>
                  UserEvent(InnerEvent(event, situatedWidget.size))
                case event => event
              }.pure
            ) *> widgetHandlesEvent(situatedWidget.value)
        )

      override def eventSource[Event](
        original: ([Event2] => DesktopWidget[Event2] => DesktopWidget[Event2]) => DesktopWidget[Event],
        onScroll: (Situated[DesktopPlacedWidget[Event]], Point2d[Float], Rect[Float]) => Event,
      ): DesktopWidget[Event] =
        trueUpdateDecoratorWithRect(
          original([Event2] => body => lower(body)),
          situatedWidget =>
            widgetHandlesEvent(situatedWidget.value)
              <* Update.updateEnvironmentalEventsM_(events =>
                val (scrollEvents, notScrollEvents) = events.partitionMap {
                  case UserEvent(event: InnerEvent) =>
                    Left((Point2d(event.event.dx, event.event.dy), event.childSize))
                  case event =>
                    Right(event)
                }
                Update
                  .emitEvents(scrollEvents.map(onScroll(situatedWidget, _, _)))
                  .as(notScrollEvents)
              )
        )

  )
end scrollEventSource
