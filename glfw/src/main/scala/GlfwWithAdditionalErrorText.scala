package me.katze.gui4s.glfw

import catnip.FailsWith
import catnip.syntax.all.{*, given}
import cats.MonadError

trait GlfwError[Window, Monitor, OriginalError, AdapredError]:
  type ErrorMapper = OriginalError => AdapredError
  def primatyMonitorGettingError : ErrorMapper
  def monitorScaleGettingError(monitor : Monitor) : ErrorMapper
end GlfwError

final class GlfwWithAdditionalErrorText[F[_, _] : FailsWith, Window, OriginalError, AdaptedError](
                                                                                                   val original : Glfw[F[OriginalError, *], Window],
                                                                                                   error : GlfwError[Window, original.Monitor, OriginalError, AdaptedError]
                                                                                                 ) extends Glfw[F[AdaptedError, *], Window]:
  override type Monitor = original.Monitor
  
  override def primaryMonitor: F[AdaptedError, Monitor] =
    original.primaryMonitor.mapError(error.primatyMonitorGettingError)
  end primaryMonitor

  override def monitorScale(monitor: original.Monitor): F[AdaptedError, Float] =
    original.monitorScale(monitor).mapError(error.monitorScaleGettingError(monitor))
  end monitorScale
end GlfwWithAdditionalErrorText


