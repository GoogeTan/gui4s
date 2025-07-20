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
  def mousePositionError(window : Window): ErrorMapper
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

  override def createOGLContext(window: Window, createCapabilities: F[AdaptedError, Unit]): F[AdaptedError, Unit] =
    original.createOGLContext(
      window, 
      createCapabilities.mapError(error.callbackError)
    ).mapError(error.oglContextCreationError(window))
  end createOGLContext

  override def pollEvents: F[AdaptedError, Unit] =
    original.pollEvents.mapError(error.eventPollingError)
  end pollEvents

  override def swapInterval(interval: Int): F[AdaptedError, Unit] =
    original.swapInterval(interval).mapError(error.swapIntervalError)
  end swapInterval

  override def primaryMonitor: F[AdaptedError, Monitor] =
    original.primaryMonitor.mapError(error.primatyMonitorGettingError)
  end primaryMonitor

  override def monitorScale(monitor: original.Monitor): F[AdaptedError, Float] =
    original.monitorScale(monitor).mapError(error.monitorScaleGettingError(monitor))
  end monitorScale

  override def createPrintErrorCallback: Resource[F[AdaptedError, *], org.lwjgl.glfw.GLFWErrorCallback] =
    original.createPrintErrorCallback.mapErrorR(error.printErrorCallbackError)

  override def primaryMonitorScale: F[AdaptedError, Float] =
    original.primaryMonitorScale.mapError(error.primatyMonitorScaleGettingError)
  end primaryMonitorScale
end GlfwWithAdditionalErrorText