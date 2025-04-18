package me.katze.gui4s.glfw

import cats.effect.Resource

trait Glfw[F[_], Window]:
  def createPrintErrorCallback : F[Unit]
  def initGlfw : F[Unit]
  def createWindow(
                    title : String,
                    size : Size,
                    visible : Boolean,
                    resizeable : Boolean,
                    debugContext : Boolean,
                  ) : Resource[F, Window]

  def centerWindow(window : Window) : F[Unit]
  def makeVisible(window : Window) : F[Unit]
  def swapInterval(interval : Int) : F[Unit]
  def createOGLContext(window : Window, createCapabilities : F[Unit]) : F[Unit]
  def shouldClose(window : Window) : F[Boolean]
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

