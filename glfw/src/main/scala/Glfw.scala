package me.katze.gui4s.glfw

import cats.Functor
import cats.effect.Resource
import org.lwjgl.glfw.GLFWErrorCallback

trait Window[F[_], Monitor]:
  def center: F[Unit]

  def makeVisible: F[Unit]

  def monitor: F[Monitor]

  def shouldClose: F[Boolean]

  def shouldNotClose(using F: Functor[F]): F[Boolean] =
    F.map(shouldClose)(a => !a)
  end shouldNotClose

  def markForBeingClosed: F[Unit]

  def size: F[Size]

  def frameBufferSize: F[Size]

  def swapBuffers: F[Unit]

  def windowResizeCallback(callback: Size => F[Unit]): F[Unit]

  def frameBufferResizeCallback(callback: Size => F[Unit]): F[Unit]

  def keyCallback(callback: (key: Int, scanCode: Int, keyAction: KeyAction, keyModes: KeyModes) => F[Unit]): F[Unit]

  def scrollCallback(callback: (xoffset: Double, yoffset: Double) => F[Unit]): F[Unit]

  def cursorPosCallback(callback: (newXPos: Double, newYPos: Double) => F[Unit]): F[Unit]

  def mouseButtonCallback(callback: (key: Int, action: KeyAction, mode: KeyModes) => F[Unit]): F[Unit]

  def currentMousePosition: F[(Double, Double)]

//TODO надо выделить окно(и, может, монитор) в отдельный интерфйс
trait Glfw[F[_], Window]:
  type Monitor
  def createPrintErrorCallback : Resource[F, GLFWErrorCallback]
  def createWindow(windowCreationSettings: WindowCreationSettings) : Resource[F, Window]
  def primaryMonitor : F[Monitor]
  def monitorScale(monitor : Monitor) : F[Float]
  def primaryMonitorScale : F[Float]
  def swapInterval(interval : Int) : F[Unit]
  def createOGLContext(window : Window, createCapabilities : F[Unit]) : F[Unit]
  def pollEvents : F[Unit]
end Glfw

