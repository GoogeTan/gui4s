package gui4s.core.widget.library.animation

import cats.Eq
import cats.derived.*

final case class AnimationState[AnimatedValue, Time](
  targetValue : AnimatedValue,
  startVelocity : AnimatedValue,
  startTime : Time,
  currentTime : Time
) derives Eq:
  def withTime(time : Time): AnimationState[AnimatedValue, Time] = 
    copy(currentTime = time)
  end withTime
end AnimationState

