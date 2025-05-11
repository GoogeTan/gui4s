package me.katze.gui4s.impure.cats
package effect

import cats.effect.Async
import cats.effect.syntax.async.*
import me.katze.gui4s.impure.Impure

import scala.concurrent.ExecutionContext

final class ContextImpure[F[_] : Async](ec : ExecutionContext, initial : Impure[F]) extends Impure[F]:
  override def impure[A](trunk: => A): F[A] =
    if Thread.currentThread().getId == 1 then
      initial.impure(trunk)
    else
      initial.impure(trunk).evalOn(ec)
  end impure
end ContextImpure
