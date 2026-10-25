package gui4s.core.layout.rowcolumn

import cats.data.State
import gui4s.core.geometry.Rect

type TestPlace[T] = State[Rect[Float], T]

def runPlace[T](value: TestPlace[T], bounds: Rect[Float]): T =
  value.runA(bounds).value
end runPlace
