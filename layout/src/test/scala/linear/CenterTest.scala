package me.katze.gui4s.layout
package linear

import org.scalatest.flatspec.AnyFlatSpec

class CenterTest extends AnyFlatSpec:
  "placeCenterMany" should "be equal to placeBeginMany + placeCenter" in:
    val widgets = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widgets) + 5
    val centreBeginCoord = placeCenter(widgets.sum, freeSpace)
    assert(placeCenterMany(widgets, freeSpace) == placeBeginMany(widgets).map(_.addBeginCoordinate(centreBeginCoord)))

  "placeCenterMany" should "have same size in the beginning and the end" in:
    val widget = List[Float](2, 5, 2)
    val freeSpace = minimalRequiredSpace(widget) + 6.0f
    assertResult((3.0f, 3.0f))(
      beginEndGaps(
        placeCenterMany(widget, freeSpace),
        freeSpace
      )
    )

  "placeCenter" should "have same size before and after" in:
    val width = 3
    val space = 5
    val res = placeCenter[Float](width, width + space + space)
    assert(res == space)
end CenterTest

