package gui4s.core.widget.library.animation

/**
 * Snap animation: instantaneous change.
 */
class SnapAnimation[A](delay: Double = 0.0)(using space: NormedVectorSpace[A]) extends Animation[A, Double]:
  def valueAtGivenMoment(playTime: Double, initialValue: A, targetValue: A, initialVelocity: A): A =
    if (playTime >= delay) targetValue else initialValue
  end valueAtGivenMoment

  def velocityAtGivenMoment(playTime: Double, initialValue: A, targetValue: A, initialVelocity: A): A =
    space.zero
  end velocityAtGivenMoment

  def duration(initialValue: A, targetValue: A, initialVelocity: A): Double =
    delay
  end duration
end SnapAnimation
