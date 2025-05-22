package me.katze.gui4s.widget
package recomposition

type ReactsOnRecomposition[Self, Recomposition] =
  (self : Self, pathToParent : Path, states : Map[String, StateTree[Recomposition]]) => Recomposition
