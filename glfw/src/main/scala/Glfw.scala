package me.katze.gui4s.glfw

import cats.Functor
import cats.effect.Resource
import org.lwjgl.glfw.GLFWErrorCallback

trait Glfw[F[_], Window]:
  type Monitor
  
  def createPrintErrorCallback : Resource[F, GLFWErrorCallback]
  
  def createWindow(windowCreationSettings: WindowCreationSettings) : Resource[F, Window]
  def centerWindow(window : Window) : F[Unit]
  def makeVisible(window : Window) : F[Unit]
  def primaryMonitor : F[Monitor]
  def windowMonitor(window : Window) : F[Monitor]
  def monitorScale(monitor : Monitor) : F[Float]
  def primaryMonitorScale : F[Float]
  def swapInterval(interval : Int) : F[Unit]
  def createOGLContext(window : Window, createCapabilities : F[Unit]) : F[Unit]
  def shouldClose(window : Window) : F[Boolean]
  
  def shouldNotClose(window: Window)(using F : Functor[F]) : F[Boolean] =
    F.map(shouldClose(window))(a => !a)
  end shouldNotClose
  
  def markForBeingClosed(window: Window) : F[Unit]
  def pollEvents : F[Unit]

  def windowResizeCallback(window: Window, callback : Size => F[Unit]) : F[Unit]
  def frameBufferResizeCallback(window: Window, callback : Size => F[Unit]) : F[Unit]
  def keyCallback(window : Window, callback : (key : Int, scanCode : Int, keyAction : KeyAction, keyModes : KeyModes) => F[Unit]) : F[Unit]
  def scrollCallback(window : Window, callback : (xoffset : Double, yoffset : Double) => F[Unit]) : F[Unit]
  def cursorPosCallback(window : Window, callback : (newXPos : Double, newYPos : Double) => F[Unit]) : F[Unit]
  def mouseButtonCallback(window : Window, callback : (key : Int, action : KeyAction, mode : KeyModes) => F[Unit]) : F[Unit]
  def currentMousePosition(window: Window): F[(Double, Double)]
  def windowSize(window : Window) : F[Size]
  def frameBufferSize(window : Window) : F[Size]
  def swapBuffers(window : Window) : F[Unit]
end Glfw

