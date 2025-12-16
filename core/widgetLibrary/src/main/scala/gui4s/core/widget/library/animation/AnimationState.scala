package gui4s.core.widget.library.animation

final case class AnimationState[AnimatedValue, Time](
  targetValue : AnimatedValue,
  startVelocity : AnimatedValue,
  startTime : Time,
  currentTime : Time
):
  def withTime(time : Time): AnimationState[AnimatedValue, Time] = 
    copy(currentTime = time)
  end withTime
end AnimationState

