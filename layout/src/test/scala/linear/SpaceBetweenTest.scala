package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec

class SpaceBetweenTest extends AnyFlatSpec:
  "Space between" should "cover all the space" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    val result = placeSpaceBetween[Float](widgets, freeSpace)
    assertResult(Rect1dOnPoint1d(freeSpace, 0f))(spaceCovered(result))

  "Space between elements" should "be distributed equally" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    assertResult(List(3, 3))(
      spaceBetweenElements(
        placeSpaceBetween[Float](widgets, freeSpace)
      )
    )
end SpaceBetweenTest

