package me.katze.gui4s.glfw

import cats.Functor
import me.katze.gui4s.geometry.{Point2d, Rect}

type GlfwWindowT[F[_], Monitor, MeasurementUnit] = [Window] =>> GlfwWindow[F, Window, Monitor, MeasurementUnit]

trait GlfwWindow[F[_], -Window, Monitor, MeasurementUnit]:
  def center(window: Window): F[Unit]

  def makeVisible(window: Window): F[Unit]

  def monitor(window: Window): F[Monitor]

  def shouldClose(window: Window): F[Boolean]

  def markForBeingClosed(window: Window): F[Unit]

  def size(window: Window): F[Rect[MeasurementUnit]]

  def frameBufferSize(window: Window): F[Rect[MeasurementUnit]]

  def swapBuffers(window: Window): F[Unit]

  def frameBufferResizeCallback(window: Window)(callback: Rect[MeasurementUnit] => F[Unit]): F[Unit]

  def keyCallback(window: Window)(callback: (key: Int, scanCode: Int, keyAction: KeyAction, keyModes: KeyModes) => F[Unit]): F[Unit]

  def scrollCallback(window: Window)(callback: (xoffset: MeasurementUnit, yoffset: MeasurementUnit) => F[Unit]): F[Unit]

  def cursorPosCallback(window: Window)(callback: (newPosition: Point2d[MeasurementUnit]) => F[Unit]): F[Unit]

  def mouseButtonCallback(window: Window)(callback: (key: Int, action: KeyAction, mode: KeyModes) => F[Unit]): F[Unit]

  def currentMousePosition(window: Window): F[Point2d[MeasurementUnit]]

  def makeContextCurrent(window: Window): F[Unit]
end GlfwWindow

object GlfwWindow:
  def apply[F[_], Window, Monitor, MeasurementUnit](using ev: GlfwWindow[F, Window, Monitor, MeasurementUnit]): GlfwWindow[F, Window, Monitor, MeasurementUnit] = ev

  extension [F[_], Window, Monitor, MeasurementUnit](window: Window)(using W: GlfwWindow[F, Window, Monitor, MeasurementUnit])
    def center: F[Unit] = W.center(window)
    def makeVisible: F[Unit] = W.makeVisible(window)
    def monitor: F[Monitor] = W.monitor(window)
    def shouldClose: F[Boolean] = W.shouldClose(window)
    def shouldNotClose(using F: Functor[F]): F[Boolean] = F.map(shouldClose)(a => !a)
    def markForBeingClosed: F[Unit] = W.markForBeingClosed(window)
    def size: F[Rect[MeasurementUnit]] = W.size(window)
    def frameBufferSize: F[Rect[MeasurementUnit]] = W.frameBufferSize(window)
    def swapBuffers: F[Unit] = W.swapBuffers(window)
    def frameBufferResizeCallback(callback: Rect[MeasurementUnit] => F[Unit]): F[Unit] = W.frameBufferResizeCallback(window)(callback)
    def keyCallback(callback: (key: Int, scanCode: Int, keyAction: KeyAction, keyModes: KeyModes) => F[Unit]): F[Unit] = W.keyCallback(window)(callback)
    def scrollCallback(callback: (xoffset: MeasurementUnit, yoffset: MeasurementUnit) => F[Unit]): F[Unit] = W.scrollCallback(window)(callback)
    def cursorPosCallback(callback: (newPosition: Point2d[MeasurementUnit]) => F[Unit]): F[Unit] = W.cursorPosCallback(window)(callback)
    def mouseButtonCallback(callback: (key: Int, action: KeyAction, mode: KeyModes) => F[Unit]): F[Unit] = W.mouseButtonCallback(window)(callback)
    def currentMousePosition: F[Point2d[MeasurementUnit]] = W.currentMousePosition(window)
    def makeContextCurrent: F[Unit] = W.makeContextCurrent(window)
end GlfwWindow
