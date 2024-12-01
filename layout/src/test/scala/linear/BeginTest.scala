package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.*
import cats.*
import cats.syntax.all.{*, given}
import org.scalatest.*

import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import scala.math.Numeric.Implicits.{*, given}
import scala.util.Random

class BeginTest extends AnyFlatSpec:
  "placeBegin" should "always return 0" in:
    assert(placeBegin[Int] == 0)
    assert(placeBegin[Float] == 0f)

  "placeBeginMany" should "form prefix sums" in:
    assert(placeBeginMany(List(2, 2, 2)) == List(0, 2, 4).map(SizedElement(2, _)))

  "placeBeginMany" should "have zero space between" in:
    val widgets = List(2, 2, 2)
    assert(
      spaceBetweenElements(
        placeBeginMany(widgets)
      ).forall(_ == 0)
    )

  "placeBeginMany with one element" should "be 0" in:
    assert(placeBeginMany(List(2)) == List(0))
end BeginTest
