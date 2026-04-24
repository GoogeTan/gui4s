package gui4s.core.widget.library.animation

import scala.math.abs 

trait NormedVectorSpace[A]:
  def zero: A
  def plus(a: A, b: A): A
  def minus(a: A, b: A): A
  def times(a: A, scalar: Double): A
  def negate(a: A): A = times(a, -1.0)
  def magnitude(a: A): Double
end NormedVectorSpace

object NormedVectorSpace:
  given doubleNormedVectorSpace: NormedVectorSpace[Double] with
    def zero: Double = 0f

    def plus(a: Double, b: Double): Double = a + b

    def minus(a: Double, b: Double): Double = a - b

    def times(a: Double, scalar: Double): Double = a * scalar

    def magnitude(a: Double): Double = abs(a.toDouble)
  end doubleNormedVectorSpace

  given floatNormedVectorSpace : NormedVectorSpace[Float] with
    def zero: Float = 0.0
    def plus(a: Float, b: Float): Float = a + b
    def minus(a: Float, b: Float): Float = a - b
    def times(a: Float, scalar: Double): Float = a * scalar.toFloat
    def magnitude(a: Float): Double = abs(a.toDouble)
  end floatNormedVectorSpace
end NormedVectorSpace