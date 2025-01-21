package me.katze.gui4s.example
package update

import me.katze.gui4s.impure.Impure


final class CatsFiber[+F[_], +E, +A](catsFiber : cats.effect.Fiber[F, E, A], impure: Impure[F]) extends me.katze.gui4s.example.task.Fiber[F]:
  override def finished: F[Boolean] =
    impure.impure:
      catsFiber.toString.contains("COMPLETED")
  end finished

  override def cancel: F[Unit] =
    catsFiber.cancel
  end cancel
end CatsFiber