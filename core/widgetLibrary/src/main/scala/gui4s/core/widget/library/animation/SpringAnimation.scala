package gui4s.core.widget.library.animation

import scala.concurrent.duration._
import scala.math._

class SpringAnimation[A](
  dampingRatio: Double = 1.0,
  stiffness: Double = 1500.0,
  visibilityThreshold: A
)(using space: NormedVectorSpace[A]) extends Animation[A, Duration] {

  private def toSeconds(d: Duration): Double = if (d.isFinite) d.toNanos / 1e9 else Double.PositiveInfinity

  private val naturalFreq = sqrt(stiffness)
  private val gamma = dampingRatio * naturalFreq
  private val discr = gamma * gamma - naturalFreq * naturalFreq
  private val isUnderDamped = dampingRatio < 1.0
  private val isOverDamped = dampingRatio > 1.0
  private val omega = if (isUnderDamped) sqrt(naturalFreq * naturalFreq - gamma * gamma) else 0.0

  def valueAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A = {
    val t = toSeconds(playTime)
    if (t <= 0) initialValue else {
      val d0 = space.minus(initialValue, targetValue)
      val v0 = initialVelocity
      val expGt = exp(-gamma * t)
      if (isUnderDamped) {
        val cosWt = cos(omega * t)
        val sinWt = sin(omega * t)
        val temp = space.plus(space.times(d0, cosWt), space.times(space.plus(v0, space.times(d0, gamma)), sinWt / omega))
        space.plus(targetValue, space.times(temp, expGt))
      } else if (dampingRatio == 1.0) {
        val temp = space.plus(d0, space.times(space.plus(v0, space.times(d0, naturalFreq)), t))
        space.plus(targetValue, space.times(temp, expGt))
      } else { // over damped
        val sqrtDiscr = sqrt(discr)
        val r1 = -gamma + sqrtDiscr
        val r2 = -gamma - sqrtDiscr
        val denom = r1 - r2
        val coef1 = space.times(space.minus(space.times(d0, r2), v0), -1.0 / denom)
        val coef2 = space.times(space.minus(v0, space.times(d0, r1)), 1.0 / denom)
        val term1 = space.times(coef1, exp(r1 * t))
        val term2 = space.times(coef2, exp(r2 * t))
        space.plus(targetValue, space.plus(term1, term2))
      }
    }
  }

  def velocityAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A = {
    val deltaTSec = 0.001
    val deltaT = Duration.fromNanos((deltaTSec * 1e9).round)
    val v1 = valueAtGivenMoment(playTime, initialValue, targetValue, initialVelocity)
    val v2 = valueAtGivenMoment(playTime + deltaT, initialValue, targetValue, initialVelocity)
    space.times(space.minus(v2, v1), 1.0 / deltaTSec)
  }

  def duration(initialValue: A, targetValue: A, initialVelocity: A): Duration = {
    var currentTime = 0.0
    val deltaTSec = 0.001
    val thresholdVel = space.times(visibilityThreshold, 1000.0) // Adjusted for seconds.
    while (true) {
      currentTime += deltaTSec
      val currentPlayTime = Duration.fromNanos((currentTime * 1e9).round)
      val currentValue = valueAtGivenMoment(currentPlayTime, initialValue, targetValue, initialVelocity)
      val currentVelocity = velocityAtGivenMoment(currentPlayTime, initialValue, targetValue, initialVelocity)
      if (space.magnitude(space.minus(currentValue, targetValue)) < space.magnitude(visibilityThreshold) &&
        space.magnitude(currentVelocity) < space.magnitude(thresholdVel)) {
        return currentPlayTime
      }
      if (currentTime > 10.0) return Duration.Inf // Safety limit: 10 seconds.
    }
    Duration.Inf // Fallback, though loop should terminate.
  }
}