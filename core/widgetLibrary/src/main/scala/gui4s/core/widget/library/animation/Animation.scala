package gui4s.core.widget.library.animation

/**
 * Этот класс описывает анимацию в общем виде.
 * @tparam AnimatedValue тип анимируемого значения
 * @tparam Time тип времени(вероятно Double или Long в наносекундах)
 */
trait Animation[AnimatedValue, Time]:
  def valueAtGivenMoment(
    playTime: Time,
    initialValue: AnimatedValue,
    targetValue: AnimatedValue,
    initialVelocity: AnimatedValue
  ): AnimatedValue

  def velocityAtGivenMoment(
    playTime: Time,
    initialValue: AnimatedValue,
    targetValue: AnimatedValue,
    initialVelocity: AnimatedValue
  ): AnimatedValue

  def duration(
    initialValue: AnimatedValue,
    targetValue: AnimatedValue,
    initialVelocity: AnimatedValue
  ): Time

  def endVelocity(
    initialValue: AnimatedValue,
    targetValue: AnimatedValue,
    initialVelocity: AnimatedValue
  ): AnimatedValue =
    velocityAtGivenMoment(
      playTime = duration(initialValue, targetValue, initialVelocity),
      initialValue = initialValue,
      targetValue = targetValue,
      initialVelocity = initialVelocity
    )//TODO check me
  end endVelocity
end Animation
