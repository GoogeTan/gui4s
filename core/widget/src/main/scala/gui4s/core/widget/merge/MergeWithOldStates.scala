package gui4s.core.widget
package merge

import cats.Functor
import cats.arrow.CommutativeArrow
import cats.arrow.Strong
import cats.syntax.all.*

type MergesWithOldStates[-Self, -RecompositionAction, +UpdatedSelf] =
  (self : Self, oldInnerStates : Map[String, StateTree[RecompositionAction]]) => UpdatedSelf

type MergesWithOldStatesF[Self, -RecompositionAction, Merge[_]] = MergesWithOldStates[Self, RecompositionAction, Merge[Self]]

given mergesWithOldStatesIsArrow[RecompositionReaction] : CommutativeArrow[[A, B] =>> MergesWithOldStates[A, RecompositionReaction, B]] with
  override def compose[A, B, C](f: MergesWithOldStates[B, RecompositionReaction, C], g: MergesWithOldStates[A, RecompositionReaction, B]): MergesWithOldStates[A, RecompositionReaction, C] =
    (self, oldStates) =>
      f(g(self, oldStates), oldStates)
  end compose

  override def first[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, B]): MergesWithOldStates[(A, C), RecompositionReaction, (B, C)] =
    (self, oldStates) =>
      (fa(self._1, oldStates), self._2)
  end first

  override def lift[A, B](f: A => B): MergesWithOldStates[A, RecompositionReaction, B] =
    (self, _) => f(self)
  end lift
end mergesWithOldStatesIsArrow

given mergesWithOldStatesIsStrong[F[_] : Functor, RecompositionReaction]: Strong[[A, B] =>> MergesWithOldStates[A, RecompositionReaction, F[B]]] with
  override def dimap[A, B, C, D](fab: MergesWithOldStates[A, RecompositionReaction, F[B]])(f: C => A)(g: B => D): MergesWithOldStates[C, RecompositionReaction, F[D]] =
    (c, event) => fab(f(c), event).map(g)
  end dimap

  override def first[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, F[B]]): MergesWithOldStates[(A, C), RecompositionReaction, F[(B, C)]] =
    (self, event) =>
      fa(self._1, event).map(b => (b, self._2))
  end first

  override def second[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, F[B]]): MergesWithOldStates[(C, A), RecompositionReaction, F[(C, B)]] =
    (self, event) =>
      fa(self._2, event).map(b => (self._1, b))
  end second
end mergesWithOldStatesIsStrong

