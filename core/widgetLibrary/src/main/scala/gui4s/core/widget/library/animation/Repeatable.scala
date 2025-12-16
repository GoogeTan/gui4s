package gui4s.core.widget.library.animation

import gui4s.core.widget.library.animation.RepeatMode.Restart


/**
 * Repeatable animation: finite repeats of another animation.
 */
class Repeatable[A](iterations: Int, animation: Animation[A, Double], repeatMode: RepeatMode = Restart)(implicit space: NormedVectorSpace[A]) extends Animation[A, Double] {

  def valueAtGivenMoment(playTime: Double, initialValue: A, targetValue: A, initialVelocity: A): A =
    if playTime <= 0 then
      initialValue
    else
      val singleDuration = animation.duration(initialValue, targetValue, initialVelocity)
      val totalDuration = iterations * singleDuration

      if playTime >= totalDuration then
        targetValue
      else
        val currentIteration = math.floor(playTime / singleDuration).toInt
        val playTimeInIteration = playTime % singleDuration
        val playForward = if (repeatMode == Restart) true else (currentIteration % 2 == 0)
        if playForward then
          animation.valueAtGivenMoment(playTimeInIteration, initialValue, targetValue, initialVelocity)
        else
          animation.valueAtGivenMoment(singleDuration - playTimeInIteration, targetValue, initialValue, space.negate(initialVelocity))
  end valueAtGivenMoment

  def velocityAtGivenMoment(playTime: Double, initialValue: A, targetValue: A, initialVelocity: A): A =
    if playTime <= 0 then
      space.zero
    else
      val singleDuration = animation.duration(initialValue, targetValue, initialVelocity)
      val totalDuration = iterations * singleDuration
      if (playTime >= totalDuration)
        space.zero
      else
        val currentIteration = math.floor(playTime / singleDuration).toInt

        val playTimeInIteration = playTime % singleDuration

        val playForward = if (repeatMode == Restart) true else (currentIteration % 2 == 0)

        if playForward then
          animation.velocityAtGivenMoment(playTimeInIteration, initialValue, targetValue, initialVelocity)
        else
          space.negate(animation.velocityAtGivenMoment(singleDuration - playTimeInIteration, targetValue, initialValue, space.negate(initialVelocity)))
  end velocityAtGivenMoment

  def duration(initialValue: A, targetValue: A, initialVelocity: A): Double = iterations * animation.duration(initialValue, targetValue, initialVelocity)
}
