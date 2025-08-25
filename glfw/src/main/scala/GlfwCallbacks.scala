package gui4s.glfw

import gui4s.core.geometry.{Point2d, Rect}

final case class GlfwCallbacks[
  F,
  MeasurementUnit
](
  onWindowResized: (newSize : Rect[MeasurementUnit]) => F,
  onMouseClick: (Int, KeyAction, KeyModes) => F,
  onMouseMove: Point2d[MeasurementUnit] => F,
  onKeyPress: (Int, Int, KeyAction, KeyModes) => F,
  onScroll : (xoffset : MeasurementUnit, yoffset : MeasurementUnit) => F
)
