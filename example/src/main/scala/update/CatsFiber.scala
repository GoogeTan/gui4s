package me.katze.gui4s.example
package update

import task.Fiber as Gui4sFiberControl

import cats.effect.Fiber as CatsEffectFiber
import me.katze.gui4s.impure.Impure


final class CatsFiber[+F[_], +E, +A](catsFiber: CatsEffectFiber[F, E, A], impure: Impure[F]) extends Gui4sFiberControl[F]:
  override def finished: F[Boolean] =
    impure.impure:
      catsFiber.toString.contains("COMPLETED") // Это ужасный код, отвратительный, но лучше способа мне не известно.
  end finished

  override def cancel: F[Unit] =
    catsFiber.cancel
  end cancel
end CatsFiber