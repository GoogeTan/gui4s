package gui4s.core.widget.library.animation

import scala.concurrent.duration._
import scala.math._

class InfiniteRepeatable[A](
  animation: Animation[A, Duration],
  repeatMode: RepeatMode = RepeatMode.Restart
)(using space: NormedVectorSpace[A]) extends Animation[A, Duration]:
  
  private def toSeconds(d: Duration): Double = if (d.isFinite) d.toNanos / 1e9 else Double.PositiveInfinity
  
  private def fromSeconds(s: Double): Duration = if (s.isInfinite) Duration.Inf else Duration.fromNanos((s * 1e9).round)

  def valueAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A = 
    if playTime <= Duration.Zero then
      initialValue 
    else 
      val singleDuration = animation.duration(initialValue, targetValue, initialVelocity)
      val singleS = toSeconds(singleDuration)
      val playS = toSeconds(playTime)
      val currentIteration = floor(playS / singleS).toInt
      var playTimeInIterationS = playS % singleS
      if (playTimeInIterationS < 0) playTimeInIterationS += singleS
      val playTimeInIteration = fromSeconds(playTimeInIterationS)
      val playForward = if repeatMode == RepeatMode.Restart then true else currentIteration % 2 == 0
      if playForward then
        animation.valueAtGivenMoment(playTimeInIteration, initialValue, targetValue, initialVelocity)
      else
        animation.valueAtGivenMoment(singleDuration - playTimeInIteration, targetValue, initialValue, space.negate(initialVelocity))
  end valueAtGivenMoment

  def velocityAtGivenMoment(playTime: Duration, initialValue: A, targetValue: A, initialVelocity: A): A =
    if playTime <= Duration.Zero then
      space.zero
    else
      val singleDuration = animation.duration(initialValue, targetValue, initialVelocity)
      val singleS = toSeconds(singleDuration)
      val playS = toSeconds(playTime)
      val currentIteration = floor(playS / singleS).toInt
      var playTimeInIterationS = playS % singleS
      if (playTimeInIterationS < 0) playTimeInIterationS += singleS
      val playTimeInIteration = fromSeconds(playTimeInIterationS)
      val playForward = if repeatMode == RepeatMode.Restart then true else currentIteration % 2 == 0
      if playForward then
        animation.velocityAtGivenMoment(playTimeInIteration, initialValue, targetValue, initialVelocity)
      else
        space.negate(animation.velocityAtGivenMoment(singleDuration - playTimeInIteration, targetValue, initialValue, space.negate(initialVelocity)))
      end if
    end if
  end velocityAtGivenMoment

  def duration(initialValue: A, targetValue: A, initialVelocity: A): Duration = Duration.Inf
end InfiniteRepeatable
