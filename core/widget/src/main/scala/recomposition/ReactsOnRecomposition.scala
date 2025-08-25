package gui4s.core.widget
package recomposition

import cats.{ContravariantMonoidal, Contravariant}
import cats.syntax.all.*
import cats.Monoid

type ReactsOnRecomposition[-Self, Recomposition] =
  (self : Self, pathToParent : Path, states : Map[String, StateTree[Recomposition]]) => Recomposition


given reactsOnRecompositionIsContravariant[Recomposition]: Contravariant[[Self] =>> ReactsOnRecomposition[Self, Recomposition]] with
  override def contramap[A, B](fa: ReactsOnRecomposition[A, Recomposition])(f: B => A): ReactsOnRecomposition[B, Recomposition] =
    (b, path, states) => fa(f(b), path, states)
end reactsOnRecompositionIsContravariant

given reactsOnRecompositionIsContravariantMonoidal[Recomposition : Monoid as RecompositionIsMonoid] : ContravariantMonoidal[[Self] =>> ReactsOnRecomposition[Self, Recomposition]] with
  override def contramap[A, B](fa: ReactsOnRecomposition[A, Recomposition])(f: B => A): ReactsOnRecomposition[B, Recomposition] =
    (b, path, states) => fa(f(b), path, states)

  override def product[A, B](fa: ReactsOnRecomposition[A, Recomposition], fb: ReactsOnRecomposition[B, Recomposition]): ReactsOnRecomposition[(A, B), Recomposition] =
    (self, path, states) => fa(self._1, path, states) |+| fb(self._2, path, states)
  end product

  override def unit: ReactsOnRecomposition[Unit, Recomposition] =
    (_, _, _) => RecompositionIsMonoid.empty
end reactsOnRecompositionIsContravariantMonoidal
