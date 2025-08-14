package me.katze.gui4s.widget
package merge

import cats.Functor
import cats.arrow.{CommutativeArrow, Strong}
import cats.syntax.all.*

type MergesWithOldStates[-Self, -RecompositionAction, +UpdatedSelf] =
  (self : Self, pathToParent : Path, oldInnerStates : Map[String, StateTree[RecompositionAction]]) => UpdatedSelf

type MergesWithOldStatesF[Self, -RecompositionAction, Merge[_]] = MergesWithOldStates[Self, RecompositionAction, Merge[Self]]

given mergesWithOldStatesIsArrow[RecompositionReaction] : CommutativeArrow[[A, B] =>> MergesWithOldStates[A, RecompositionReaction, B]] with
  override def compose[A, B, C](f: MergesWithOldStates[B, RecompositionReaction, C], g: MergesWithOldStates[A, RecompositionReaction, B]): MergesWithOldStates[A, RecompositionReaction, C] =
    (self, path, oldStates) =>
      f(g(self, path, oldStates), path, oldStates)
  end compose

  override def first[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, B]): MergesWithOldStates[(A, C), RecompositionReaction, (B, C)] =
    (self, path, oldStates) =>
      (fa(self._1, path, oldStates), self._2)
  end first

  override def lift[A, B](f: A => B): MergesWithOldStates[A, RecompositionReaction, B] =
    (self, _, _) => f(self)
  end lift
end mergesWithOldStatesIsArrow

given mergesWithOldStatesIsStrong[F[_] : Functor, RecompositionReaction]: Strong[[A, B] =>> MergesWithOldStates[A, RecompositionReaction, F[B]]] with
  override def dimap[A, B, C, D](fab: MergesWithOldStates[A, RecompositionReaction, F[B]])(f: C => A)(g: B => D): MergesWithOldStates[C, RecompositionReaction, F[D]] =
    (c, path, event) => fab(f(c), path, event).map(g)
  end dimap

  override def first[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, F[B]]): MergesWithOldStates[(A, C), RecompositionReaction, F[(B, C)]] =
    (self, path, event) =>
      fa(self._1, path, event).map(b => (b, self._2))
  end first

  override def second[A, B, C](fa: MergesWithOldStates[A, RecompositionReaction, F[B]]): MergesWithOldStates[(C, A), RecompositionReaction, F[(C, B)]] =
    (self, path, event) =>
      fa(self._2, path, event).map(b => (self._1, b))
  end second
end mergesWithOldStatesIsStrong

