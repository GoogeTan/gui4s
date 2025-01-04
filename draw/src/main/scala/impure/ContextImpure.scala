package me.katze.gui4s.draw
package impure

import cats.effect.syntax.all.*
import cats.effect.{Async, IO}

import scala.concurrent.ExecutionContext

final class ContextImpure[F[_] : Async](ec : ExecutionContext, initial : Impure[F]) extends Impure[F]:
  override def impure[A](trunk: => A): F[A] =
    initial.impure(trunk).evalOn(ec)
  end impure
end ContextImpure
