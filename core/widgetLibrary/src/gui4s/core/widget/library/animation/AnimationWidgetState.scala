package gui4s.core.widget.library.animation

import cats.Eq
import cats.Group
import cats.syntax.all.*

final case class AnimationWidgetState[
  AnimatedValue,
  Time
](
  startValue : AnimatedValue,
  animation: Animation[AnimatedValue, Time],
  playingAnimation : Option[AnimationState[AnimatedValue, Time]]
):
  def withTime(time: Time)(using Group[Time], Ordering[Time]) : AnimationWidgetState[AnimatedValue, Time] =
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

  def isAnimationFinished(using G : Group[Time], O : Ordering[Time]) : Boolean =
    playingAnimation.fold(true)(state =>
      O.gt(
        G.remove(state.currentTime, state.startTime),
        animation.duration(
          initialValue = startValue,
          targetValue = state.targetValue,
          initialVelocity = state.startVelocity
        )
      )
    )
  end isAnimationFinished
end AnimationWidgetState

object AnimationWidgetState:
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))//Мы хотим здесь сравнение по ссылке считай
  given[AnimatedValue : Eq, Time : Eq]: Eq[AnimationWidgetState[AnimatedValue, Time]] with
    override def eqv(x: AnimationWidgetState[AnimatedValue, Time], y: AnimationWidgetState[AnimatedValue, Time]): Boolean =
      x.startValue === y.startValue
        && x.animation == y.animation
        && x.playingAnimation === y.playingAnimation
    end eqv
  end given
end AnimationWidgetState
