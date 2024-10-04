package me.katze.gui4s.example
package task

import cats.effect.Fiber

final case class IOOnThread[F[_]](keepAfterWidgetDeath: Boolean, fiberControl: Fiber[F, Throwable, Unit])
