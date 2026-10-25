package gui4s.core.layout
package linear

import gui4s.core.geometry.*
import org.scalatest.flatspec.AnyFlatSpec

class SpaceAroundTest extends AnyFlatSpec:
  "placeSpaceAround" should "cover all the space except begin-end gaps" in:
    val widgets = List[Float](2, 2, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6f
    val spaceAround = spaceBetween[List, Float]((0f :: widgets) :+ 0f, freeSpace)
    assertResult(
      Rect1dOnPoint1d.fromStartAndEnd(spaceAround, freeSpace - spaceAround)
    )(
      spaceCovered(
        placeSpaceAround(widgets, freeSpace)
      )
    )
  
  "placeSpaceAround" should "keep gaps equal" in:
    val widgets = List[Float](2, 2, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6f
    assert(
      spaceAroundElements(
        placeSpaceAround(widgets, freeSpace), 
        freeSpace
      ).toNes.length == 1
    )
end SpaceAroundTest

