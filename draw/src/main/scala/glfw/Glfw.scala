package me.katze.gui4s.draw
package glfw

import cats.effect.Resource

trait Glfw[F[_]]:
  type Window

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
  def setupDebugMessageCallback : F[Unit]
  def swapInterval(interval : Int) : F[Unit]
  def createOGLContext(window : Window) : F[Unit]
  def shouldClose(window : Window) : F[Boolean]
  def markForBeingClosed(window: Window) : F[Unit]

  def pollEvents : F[Unit]

  def windowResizeCallback(window: Window, callback : Size => F[Unit]) : F[Unit]
  def frameBufferResizeCallback(window: Window, callback : Size => F[Unit]) : F[Unit]
  def keyCallback(window : Window, callback : (Int, Int, KeyAction, KeyModes) => F[Unit]) : F[Unit]

  def windowSize(window : Window) : F[Size]
  def frameBufferSize(window : Window) : F[Size]
end Glfw

