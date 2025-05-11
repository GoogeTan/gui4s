package me.katze.gui4s.glfw

import cats.Functor
import cats.effect.Resource
import org.lwjgl.glfw.GLFWErrorCallback

trait Glfw[F[_], Window]:
  type Monitor
  
  def createPrintErrorCallback : Resource[F, GLFWErrorCallback]
  
  def createWindow(
                    title : String,
                    size : Size,
                    visible : Boolean,
                    resizeable : Boolean,
                    debugContext : Boolean,
                  ) : Resource[F, Window]

  def centerWindow(window : Window) : F[Unit]
  def makeVisible(window : Window) : F[Unit]
  def currentMonitor : F[Monitor]
  def monitorScale(monitor : Monitor) : F[Float]
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
  def keyCallback(window : Window, callback : (Int, Int, KeyAction, KeyModes) => F[Unit]) : F[Unit]
  def scrollCallback(window : Window, callback : (Double, Double) => F[Unit]) : F[Unit]
  def cursorPosCallback(window : Window, callback : (Double, Double) => F[Unit]) : F[Unit]
  def mouseButtonCallback(window : Window, callback : (Int, KeyAction, KeyModes) => F[Unit]) : F[Unit]
  
  def windowSize(window : Window) : F[Size]
  
  def frameBufferSize(window : Window) : F[Size]
  def swapBuffers(window : Window) : F[Unit]
end Glfw

