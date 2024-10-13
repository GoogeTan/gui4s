package me.katze.gui4s.example
package draw

import cats.Apply
import cats.implicits.catsSyntaxApplyOps

// TODO notify draw thread about changes
final class NotifyDrawLoopWindow[F[_] : Apply, MU](initial: Window[F, MU], notifySizeChanged: F[Unit]) extends Window[F, MU]:
  override def enterFullScreen: F[Unit] =
    initial.enterFullScreen *> notifySizeChanged
  end enterFullScreen
  
  override def resize(width: MU, height: MU): F[Unit] =
    initial.resize(width, height) *> notifySizeChanged
  end resize

  override def size: F[(MU, MU)] = initial.size
end NotifyDrawLoopWindow
  