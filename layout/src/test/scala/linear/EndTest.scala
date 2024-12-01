package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec

class EndTest extends AnyFlatSpec:
  "placeEndMany" should "be equal to placeBeginMany + placeEnd" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 5
    val endBeginCoord = placeEnd(widgets.sum, freeSpace)
    assert(placeEndMany(widgets, freeSpace) == placeBeginMany(widgets).map(_ .addBeginCoordinate(endBeginCoord)))

  "placeEndMany" should "keep space between zero" in:
    val widgets = List(2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 5
    assert(spaceBetweenElements(placeEndMany(widgets, freeSpace)).forall(_ == 0))
end EndTest

