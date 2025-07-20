package me.katze.gui4s.widget
package state

import cats.ContravariantMonoidal

type HasInnerStates[-T, +RecompositionReaction] =
  T => Map[String, StateTree[RecompositionReaction]]

given hasInnerStatesIsContravariantMonoidal[RecompositionReaction]: ContravariantMonoidal[HasInnerStates[*, RecompositionReaction]] with
  override def contramap[A, B](fa: HasInnerStates[A, RecompositionReaction])(f: B => A): HasInnerStates[B, RecompositionReaction] =
    b => fa(f(b))
  end contramap

  override def unit: HasInnerStates[Unit, RecompositionReaction] =
    _ => Map()
  end unit

  override def product[A, B](fa: HasInnerStates[A, RecompositionReaction], fb: HasInnerStates[B, RecompositionReaction]): HasInnerStates[(A, B), RecompositionReaction] =
    (a, b) =>
      fa(a) ++ fb(b)
  end product
end hasInnerStatesIsContravariantMonoidal
