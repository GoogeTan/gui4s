package me.katze.gui4s.example
package application

import cats.effect.IO
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.stateful.Path

trait IOQueue[F[+_]]:
  def push[T](returnPath : Path, task : WidgetTaskImpl[F, T]) : F[Unit]
end IOQueue
