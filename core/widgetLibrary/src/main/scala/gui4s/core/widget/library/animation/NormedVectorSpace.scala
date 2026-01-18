package gui4s.core.widget.library.animation

import scala.Numeric.Implicits.given
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
  given numericNormedVectorSpace[T : Numeric as N] : NormedVectorSpace[T] with
    def zero: T = N.zero
    def plus(a: T, b: T): T = a + b
    def minus(a: T, b: T): T = a - b
    def times(a: T, scalar: Double): T = a * N.parseString(scalar.toString).get//TODO remove this thing
    def magnitude(a: T): Double = abs(a.toDouble)
  end numericNormedVectorSpace
end NormedVectorSpace