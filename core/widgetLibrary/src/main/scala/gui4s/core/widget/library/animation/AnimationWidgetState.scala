package gui4s.core.widget.library.animation

import cats.{Group, Order}
import cats.syntax.all.*

final case class AnimationWidgetState[
  AnimatedValue,
  Time
](
  startValue : AnimatedValue,
  animation: Animation[AnimatedValue, Time],
  playingAnimation : Option[AnimationState[AnimatedValue, Time]]
):
  def withTime(time: Time)(using Group[Time], Order[Time]) : AnimationWidgetState[AnimatedValue, Time] =
    playingAnimation.fold(this)(animationState =>
      val newState = this.copy(playingAnimation = Some(animationState.withTime(time)))
      if newState.isAnimationFinished then
        this.copy(playingAnimation = None, startValue = animationState.targetValue)
      else
        newState
    )
  end withTime

  def valueNow(using G : Group[Time]) : AnimatedValue =
    playingAnimation.fold(
      startValue
    )(
      animationState =>
        animation.valueAtGivenMoment(
          playTime = G.remove(animationState.currentTime, animationState.startTime),
          initialValue = startValue,
          targetValue = animationState.targetValue,
          initialVelocity = animationState.startVelocity
        )
    )
  end valueNow
  
  def valueAtMoment(time : Time)(using G : Group[Time]) : AnimatedValue =
    playingAnimation.fold(
      startValue
    )(
        animationState => 
          animation.valueAtGivenMoment(
            playTime = G.remove(time, animationState.startTime),
            initialValue = startValue,
            targetValue = animationState.targetValue,
            initialVelocity = animationState.startVelocity 
          )
    )
  end valueAtMoment  
  
  def velocityAtMoment(time : Time) : Option[AnimatedValue] = 
      playingAnimation.map(
        animationState => 
            animation.velocityAtGivenMoment(
              playTime = time,
              initialValue = startValue,
              targetValue = animationState.targetValue,
              initialVelocity = animationState.startVelocity
            )
      )
  end velocityAtMoment

  def isAnimationFinished(using G : Group[Time], O : Order[Time]) : Boolean =
    playingAnimation.fold(true)(state =>
      G.remove(state.currentTime, state.startTime)
        > animation.duration(
        initialValue = startValue,
        targetValue = state.targetValue,
        initialVelocity = state.startVelocity
      )
    )
  end isAnimationFinished
end AnimationWidgetState

