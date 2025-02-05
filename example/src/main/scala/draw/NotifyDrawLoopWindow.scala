package me.katze.gui4s.example
package draw

import cats.Apply
import cats.implicits.catsSyntaxApplyOps

final class NotifyDrawLoopWindow[F[_] : Apply, MeasurementUnit](initial: Window[F, MeasurementUnit], notifySizeChanged: F[Unit]) extends Window[F, MeasurementUnit]:
  override def enterFullScreen: F[Unit] =
    initial.enterFullScreen *> notifySizeChanged
  end enterFullScreen
  
  override def resize(width: MeasurementUnit, height: MeasurementUnit): F[Unit] =
    initial.resize(width, height) *> notifySizeChanged
  end resize

  override def size: F[(MeasurementUnit, MeasurementUnit)] = initial.size

  override def onResizedByUser: F[Unit] =
    initial.onResizedByUser *> notifySizeChanged
  end onResizedByUser
end NotifyDrawLoopWindow
  