package me.katze.gui4s.example

import me.katze.gui4s.widget.RunnableIO
import me.katze.gui4s.widget.stateful.Path

trait IOMaster[F[_], WidgetTask]:
  def pushIO(io : RunnableIO[WidgetTask]): F[Unit]
  
  def alive: F[Set[Path]]
  
  def detach(path: Path): F[Unit]
end IOMaster
