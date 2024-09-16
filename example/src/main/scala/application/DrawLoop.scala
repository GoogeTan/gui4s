package me.katze.gui4s.example
package application

import cats.effect.ExitCode
import cats.effect.kernel.RefSource
import me.katze.gui4s.widget.PlacedWidget

trait DrawLoop[F[+_], Draw]:
  def drawLoop(widget: RefSource[F, ? <: PlacedWidget[Draw, ?, ?, ?, ?]]) : F[ExitCode]
end DrawLoop

type DrawLoopT[Draw] = [F[+_]] =>> DrawLoop[F,  Draw]

def drawLoop[F[+_], Draw](widget: RefSource[F, ? <: PlacedWidget[Draw, ?, ?, ?, ?]])(using D: DrawLoop[F, Draw]): F[ExitCode] =
  D.drawLoop(widget)
end drawLoop
