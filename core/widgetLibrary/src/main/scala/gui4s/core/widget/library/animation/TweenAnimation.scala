package gui4s.core.widget.library.animation

import scala.concurrent.duration._

class TweenAnimation[A](
  duration: Duration,
  easing: Easing = Easing.Linear,
  delay: Duration = Duration.Zero
)(using space: NormedVectorSpace[A]) extends Animation[A, Duration]:

  private def toSeconds(d: Duration): Double = if (d.isFinite) d.toNanos / 1e9 else Double.PositiveInfinity

  def valueAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A =
    val playS = toSeconds(playTime)
    val delayS = toSeconds(delay)
    val durS = toSeconds(duration)
    if playS < delayS then
      initialValue
    else
      val time = playS - delayS
      if time >= durS then
        targetValue
      else
        val fraction = easing.transform(time / durS)
        space.plus(initialValue, space.times(space.minus(targetValue, initialValue), fraction))
      end if
    end if
  end valueAtGivenMoment

  def velocityAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A =
    val deltaTSec = 0.001
    val deltaT = Duration.fromNanos((deltaTSec * 1e9).round)
    val v1 = valueAtGivenMoment(playTime, initialValue, targetValue, initialVelocity)
    val v2 = valueAtGivenMoment(playTime + deltaT, initialValue, targetValue, initialVelocity)
    space.times(space.minus(v2, v1), 1.0 / deltaTSec)
  end velocityAtGivenMoment

  def duration(initialValue: A, targetValue: A, initialVelocity: A): Duration =
    delay + duration
  end duration
end TweenAnimation
