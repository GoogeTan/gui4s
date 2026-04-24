package gui4s.desktop.kit.widgets

import scala.concurrent.duration.Duration

import cats.effect.IO
import cats.syntax.all.*

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.widget.library.animation.Animation
import gui4s.core.widget.library.animation.AnimationWidget

import gui4s.desktop.kit.effects.*

final case class RichInnerEvent[MouseEvent](scrollChange: MouseEvent, childSize: Rect[Float], ownSize: Rect[Float])

trait ScrollWidget:
  def apply[Event](
                    original : DesktopWidget[Event],
                    direction : Axis,
                    stateName : String,
                    scrollAnimation: Animation[Float, Duration],
                    scrollSpeed : Float = 8f
                  ) : DesktopWidget[Event]

  extension[Event](original : DesktopWidget[Event])
    def scrollable(
      direction : Axis, 
      stateName : String,
      scrollAnimation: Animation[Float, Duration],
      scrollSpeed : Float = 8f
    ) : DesktopWidget[Event] =
      apply(original, direction, stateName, scrollAnimation, scrollSpeed)
    end scrollable

    def verticallyScrollable(
      stateName: String, 
      scrollAnimation: Animation[Float, Duration],
      scrollSpeed : Float = 8f
    ): DesktopWidget[Event] =
      apply(original, Axis.Vertical, stateName, scrollAnimation, scrollSpeed)
    end verticallyScrollable

    def horizontallyScrollable(
      stateName: String, 
      scrollAnimation: Animation[Float, Duration],
      scrollSpeed : Float = 8f
    ): DesktopWidget[Event] =
      apply(original, Axis.Horizontal, stateName, scrollAnimation, scrollSpeed)
    end horizontallyScrollable
  end extension
end ScrollWidget

//TODO Пока что нельзя из кода инициировать скролл к нужному месту. Надо написать документацию про то, как поглощаются события для работы вложенных скроллов
def scrollWidget[ScrollEvent](
  eventSource: ScrollEventSource[ScrollEvent],
  animation: AnimationWidget[DesktopWidget, Float, Duration]
) : ScrollWidget =
  new ScrollWidget:
    override def apply[Event](
                               original: DesktopWidget[Event],
                               direction: Axis,
                               stateName: String,
                               animationConfig: Animation[Float, Duration],
                               scrollSpeed : Float
                             ): DesktopWidget[Event] =
      transitiveStatefulWidget[Float, Event, RichInnerEvent[ScrollEvent]](
        name = stateName,
        initialState = 0f,
        eventHandler = (state, events) =>
          events.foldM(state) {
            case (currentState, RichInnerEvent(scrollEvent, childSize, ownSize)) =>
              updateScrollAccordingToTheEvent(using eventSource)(
                ownSize,
                childSize,
                scrollEvent,
                currentState,
                direction,
                scrollSpeed
              )
          },
        body = count =>
          animation(
            name = "scroll_animation",
            targetValue = count,
            animation = animationConfig,
            body = scrollValue =>
              eventSource.eventSource(lower =>
                lightScrollWidget(
                  body = lower(original),
                  direction = direction,
                  shiftProvider = body =>
                    val shiftValue = scrollValue * scrollSpeed
                    val shift = direction match
                      case Axis.Horizontal => Point3d(shiftValue, 0f, 0f)
                      case Axis.Vertical => Point3d(0f, shiftValue, 0f)
                    body(shift),
                  boundsForClip = scrollAreaSize =>
                    PlacementEffect.getBounds.map(bounds => Shape.rect(bounds.map(_.value.getOrElse(1000000)))(scrollAreaSize))
                ).mapEvent(Right(_)),
                (sizedChild, scrollShift, childSize) =>
                  Left(RichInnerEvent(scrollShift, childSize, sizedChild.size))
              )
          )
      )
    end apply
  end new
end scrollWidget

def updateScrollAccordingToTheEvent[Event, ScrollEvent](
  using eventSource: ScrollEventSource[ScrollEvent]
)(
  ownSize : Rect[Float],
  childSize : Rect[Float],
  scrollEvent : ScrollEvent,
  currentState : Float,
  direction : Axis,
  scrollSpeed : Float,
) : Update[Event, Float] =
  val minScroll = (ownSize.along(direction) - childSize.along(direction)).min(0f) / scrollSpeed
  val nextState = currentState + scrollEvent.shiftAlong(direction)
  val clampedState = nextState.max(minScroll).min(0f)
  val leftover = if nextState < minScroll then
    nextState - minScroll
  else if nextState > 0f then
    nextState
  else
    0f

  (
    if leftover != 0f then
      eventSource.emitLeftovers(scrollEvent.withShiftAlong(direction, leftover))
    else
      ().pure[Update[Event, *]]
    ).as(clampedState)
end updateScrollAccordingToTheEvent
