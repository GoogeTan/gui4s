package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec

class SpaceBetweenTest extends AnyFlatSpec:
  "Space between" should "cover all the space" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    val result = placeSpaceBetween[Float](widgets, freeSpace)
    assertResult(CoveredSpace(0, freeSpace))(spaceCovered(result, widgets))

  "Space between elements" should "be distributed equally" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    assertResult(List(3, 3))(
      spaceBetweenElements(
        placeSpaceBetween[Float](widgets, freeSpace),
        widgets,
      )
    )
end SpaceBetweenTest

