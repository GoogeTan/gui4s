package me.katze.gui4s.widget
package merge

import cats.Functor
import cats.arrow.{Arrow, CommutativeArrow, Strong}
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

given mergesWithOldStatesIsStrong[F[_] : Functor, Event]: Strong[[A, B] =>> MergesWithOldStates[A, Event, F[B]]] with
  override def dimap[A, B, C, D](fab: MergesWithOldStates[A, Event, F[B]])(f: C => A)(g: B => D): MergesWithOldStates[C, Event, F[D]] =
    (c, path, event) => fab(f(c), path, event).map(g)
  end dimap

  override def first[A, B, C](fa: MergesWithOldStates[A, Event, F[B]]): MergesWithOldStates[(A, C), Event, F[(B, C)]] =
    (self, path, event) =>
      fa(self._1, path, event).map(b => (b, self._2))
  end first

  override def second[A, B, C](fa: MergesWithOldStates[A, Event, F[B]]): MergesWithOldStates[(C, A), Event, F[(C, B)]] =
    (self, path, event) =>
      fa(self._2, path, event).map(b => (self._1, b))
  end second
end mergesWithOldStatesIsStrong

