package gui4s.core.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec
import gui4s.core.geometry.*

class SpaceBetweenTest extends AnyFlatSpec:
  "Space between" should "cover all the space" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    val result = placeSpaceBetween[List, Float](widgets, freeSpace)
    assertResult(Rect1dOnPoint1d(freeSpace, 0f))(spaceCovered(result))

  "Space between elements" should "be distributed equally" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6.0f
    assertResult(List(3, 3))(
      spaceBetweenElements(
        placeSpaceBetween[List, Float](widgets, freeSpace)
      )
    )
end SpaceBetweenTest

