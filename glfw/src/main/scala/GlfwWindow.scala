package me.katze.gui4s.glfw

import cats.Functor
import me.katze.gui4s.geometry.{Point2d, Rect}

trait GlfwWindow[F[_], Monitor, MeasurementUnit]:
  def center: F[Unit]

  def makeVisible: F[Unit]

  def monitor: F[Monitor]

  def shouldClose: F[Boolean]

  def shouldNotClose(using F: Functor[F]): F[Boolean] =
    F.map(shouldClose)(a => !a)
  end shouldNotClose

  def markForBeingClosed: F[Unit]

  def size: F[Rect[MeasurementUnit]]

  def frameBufferSize: F[Rect[MeasurementUnit]]

  def swapBuffers: F[Unit]

  def windowResizeCallback(callback: Rect[MeasurementUnit] => F[Unit]): F[Unit]

  def frameBufferResizeCallback(callback: Rect[MeasurementUnit] => F[Unit]): F[Unit]

  def keyCallback(callback: (key: Int, scanCode: Int, keyAction: KeyAction, keyModes: KeyModes) => F[Unit]): F[Unit]

  def scrollCallback(callback: (xoffset: MeasurementUnit, yoffset: MeasurementUnit) => F[Unit]): F[Unit]

  def cursorPosCallback(callback:  (newPosition : Point2d[MeasurementUnit]) => F[Unit]): F[Unit]

  def mouseButtonCallback(callback: (key: Int, action: KeyAction, mode: KeyModes) => F[Unit]): F[Unit]

  def currentMousePosition: F[Point2d[MeasurementUnit]]

  def makeContextCurrent : F[Unit]
end GlfwWindow
