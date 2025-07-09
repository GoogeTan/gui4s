package me.katze.gui4s.glfw

import catnip.FailsWith
import catnip.cats.effect.syntax.all.{*, given}
import catnip.syntax.all.{*, given}
import cats.effect.Resource

trait GlfwError[Window, Monitor, OriginalError, AdaptedError]:
  type ErrorMapper = OriginalError => AdaptedError
  
  def primatyMonitorGettingError: ErrorMapper
  def primatyMonitorScaleGettingError: ErrorMapper
  def monitorScaleGettingError(monitor: Monitor): ErrorMapper
  def windowCreationError: ErrorMapper
  def windowOperationError(window: Window): ErrorMapper
  def windowSizeError(window: Window): ErrorMapper
  def windowMonitorError(window: Window): ErrorMapper
  def oglContextCreationError(window: Window): ErrorMapper
  def callbackRegistrationError(window: Window): ErrorMapper
  def eventPollingError: ErrorMapper
  def bufferOperationError(window: Window): ErrorMapper
  def frameBufferError(window: Window): ErrorMapper
  def swapIntervalError: ErrorMapper
  def visibilityError(window: Window): ErrorMapper
  def callbackError : AdaptedError => OriginalError
  def printErrorCallbackError: ErrorMapper
  def mouseButtonCallbackError(window: Window): ErrorMapper
  def scrollCallbackError(window: Window): ErrorMapper
  def windowResizeCallbackError(window: Window): ErrorMapper
end GlfwError

final class GlfwWithAdditionalErrorText[F[_, _] : {FailsWith, BiMonadCancel}, Window, OriginalError, AdaptedError](
    val original: Glfw[F[OriginalError, *], Window],
    error: GlfwError[Window, original.Monitor, OriginalError, AdaptedError]
) extends Glfw[F[AdaptedError, *], Window]:
  override type Monitor = original.Monitor

  override def createWindow(settings: WindowCreationSettings): Resource[F[AdaptedError, *], Window] =
    original.createWindow(settings)
      .mapErrorR(error.windowCreationError)
  end createWindow

  override def centerWindow(window: Window): F[AdaptedError, Unit] =
    original.centerWindow(window).mapError(error.windowOperationError(window))
  end centerWindow

  override def createOGLContext(window: Window, createCapabilities: F[AdaptedError, Unit]): F[AdaptedError, Unit] =
    original.createOGLContext(
      window, 
      createCapabilities.mapError(error.callbackError)
    ).mapError(error.oglContextCreationError(window))
  end createOGLContext

  override def cursorPosCallback(window: Window, callback: (newXPos : Double, newYPos : Double) => F[AdaptedError, Unit]): F[AdaptedError, Unit] =
    original.cursorPosCallback(
      window, 
      (x, y) => callback(x, y).mapError(error.callbackError)
    ).mapError(error.callbackRegistrationError(window))
  end cursorPosCallback

  override def frameBufferResizeCallback(window: Window, callback: Size => F[AdaptedError, Unit]): F[AdaptedError, Unit] =
    original.frameBufferResizeCallback(
      window,
      size => callback(size).mapError(error.callbackError)
    ).mapError(error.callbackRegistrationError(window))
  end frameBufferResizeCallback

  override def frameBufferSize(window: Window): F[AdaptedError, Size] =
    original.frameBufferSize(window).mapError(error.frameBufferError(window))
  end frameBufferSize

  override def keyCallback(
      window: Window, 
      callback: (key : Int, scanCode : Int, keyAction : KeyAction, keyModes : KeyModes) => F[AdaptedError, Unit]
  ): F[AdaptedError, Unit] =
    original.keyCallback(
      window, 
      (key, scanCode, keyAction, keyModes) => callback(key, scanCode, keyAction, keyModes).mapError(error.callbackError)
    ).mapError(error.callbackRegistrationError(window))
  end keyCallback

  override def makeVisible(window: Window): F[AdaptedError, Unit] =
    original.makeVisible(window).mapError(error.visibilityError(window))
  end makeVisible

  override def markForBeingClosed(window: Window): F[AdaptedError, Unit] =
    original.markForBeingClosed(window).mapError(error.windowOperationError(window))
  end markForBeingClosed

  override def pollEvents: F[AdaptedError, Unit] =
    original.pollEvents.mapError(error.eventPollingError)
  end pollEvents

  override def shouldClose(window: Window): F[AdaptedError, Boolean] =
    original.shouldClose(window).mapError(error.windowOperationError(window))
  end shouldClose

  override def swapBuffers(window: Window): F[AdaptedError, Unit] =
    original.swapBuffers(window).mapError(error.bufferOperationError(window))
  end swapBuffers

  override def swapInterval(interval: Int): F[AdaptedError, Unit] =
    original.swapInterval(interval).mapError(error.swapIntervalError)
  end swapInterval

  override def windowMonitor(window: Window): F[AdaptedError, Monitor] =
    original.windowMonitor(window).mapError(error.windowMonitorError(window))
  end windowMonitor

  override def windowSize(window: Window): F[AdaptedError, Size] =
    original.windowSize(window).mapError(error.windowSizeError(window))
  end windowSize

  override def primaryMonitor: F[AdaptedError, Monitor] =
    original.primaryMonitor.mapError(error.primatyMonitorGettingError)
  end primaryMonitor

  override def monitorScale(monitor: original.Monitor): F[AdaptedError, Float] =
    original.monitorScale(monitor).mapError(error.monitorScaleGettingError(monitor))
  end monitorScale

  override def createPrintErrorCallback: Resource[F[AdaptedError, *], org.lwjgl.glfw.GLFWErrorCallback] =
    original.createPrintErrorCallback.mapErrorR(error.printErrorCallbackError)

  override def mouseButtonCallback(window: Window, callback: (key : Int, action : KeyAction, mode : KeyModes) => F[AdaptedError, Unit]): F[AdaptedError, Unit] =
    original.mouseButtonCallback(
      window,
      (button, action, mods) => callback(button, action, mods).mapError(error.callbackError)
    ).mapError(error.mouseButtonCallbackError(window))
  end mouseButtonCallback

  override def scrollCallback(
      window: Window,
      callback: (xoffset : Double, yoffset : Double) => F[AdaptedError, Unit]
  ): F[AdaptedError, Unit] =
    original.scrollCallback(
      window,
      (xOffset, yOffset) => callback(xOffset, yOffset).mapError(error.callbackError)
    ).mapError(error.scrollCallbackError(window))
  end scrollCallback

  override def windowResizeCallback(
      window: Window,
      callback: Size => F[AdaptedError, Unit]
  ): F[AdaptedError, Unit] =
    original.windowResizeCallback(
      window,
      size => callback(size).mapError(error.callbackError)
    ).mapError(error.windowResizeCallbackError(window))
  end windowResizeCallback

  override def primaryMonitorScale: F[AdaptedError, Float] =
    original.primaryMonitorScale.mapError(error.primatyMonitorScaleGettingError)
  end primaryMonitorScale
end GlfwWithAdditionalErrorText