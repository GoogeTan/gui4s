package catnip.cats.effect
package syntax

import syntax.all.{BiMonadCancel, given}

import catnip.FailsWith
import catnip.syntax.all.{*, given}
import cats.*
import cats.arrow.FunctionK
import cats.effect.Resource

object fails:
  def mapErrorLift[F[_, _] : FailsWith, E1, E2](f : E1 => E2) : F[E1, *] ~> F[E2, *] =
    new FunctionK[F[E1, *], F[E2, *]]:
      override def apply[A](a : F[E1, A]) : F[E2, A] =
        a.mapError(f)
      end apply
    end new

  extension [F[_, _] : {FailsWith, BiMonadCancel}, E1, T](value : Resource[F[E1, *], T])
    def mapErrorR[E2](f : E1 => E2) : Resource[F[E2, *], T] =
      value.mapK(mapErrorLift(f))
    end mapErrorR
  end extension
end fails
