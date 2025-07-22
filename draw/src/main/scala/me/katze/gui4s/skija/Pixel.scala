package me.katze.gui4s.skija

final case class Pixel private (private val amount : Float):
  def toFloat : Float = amount

object Pixel:
  def apply(fl: Float): Pixel =
    new Pixel(fl)
  end apply

  given Fractional[Pixel] with
    override def plus(x: Pixel, y: Pixel): Pixel = Pixel(x.amount + y.amount)

    override def div(x: Pixel, y: Pixel): Pixel = Pixel(x.amount / y.amount)

    override def minus(x: Pixel, y: Pixel): Pixel = Pixel(x.amount - y.amount)

    override def times(x: Pixel, y: Pixel): Pixel = Pixel(x.amount * y.amount)

    override def negate(x: Pixel): Pixel = Pixel(-x.amount)

    override def fromInt(x: Int): Pixel = Pixel(x.toFloat)

    override def parseString(str: String): Option[Pixel] = str.toFloatOption.map(Pixel(_))

    override def toInt(x: Pixel): Int = x.amount.toInt

    override def toLong(x: Pixel): Long = x.amount.toLong

    override def toFloat(x: Pixel): Float = x.amount.toFloat

    override def toDouble(x: Pixel): Double = x.amount.toLong

    override def compare(x: Pixel, y: Pixel): Int = x.amount.compare(y.amount)
  end given
end Pixel
