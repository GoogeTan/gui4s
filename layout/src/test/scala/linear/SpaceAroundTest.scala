package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec

class SpaceAroundTest extends AnyFlatSpec:
  "placeSpaceAround" should "cover all the space except begin-end gaps" in:
    val widgets = List[Float](2, 2, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 6f
    val spaceAround = spaceBetween[Float]((0f :: widgets) :+ 0f, freeSpace)
    assertResult(
      CoveredSpace(spaceAround, freeSpace - spaceAround)
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
      ).toSet.size == 1
    )
end SpaceAroundTest

