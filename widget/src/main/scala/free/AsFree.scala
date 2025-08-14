package me.katze.gui4s.widget
package free

import cats.Functor
import cats.syntax.all.{*, given}

type AsFreeF[Placed, F[_]] = AsFree[Placed, F[Placed]]

type AsFree[-Placed, +Free] = (self : Placed) => Free

def pairedAsFreeRight[A, B, F[_] : Functor](asFree : AsFreeF[B, F]) : AsFreeF[(A, B), F] =
  (a, b) => asFree(b).map(newB => (a, newB))
end pairedAsFreeRight

def pairedAsFreeLeft[A, B, F[_] : Functor](asFree : AsFreeF[A, F]) : AsFreeF[(A, B), F] =
  (a, b) => asFree(a).map(newA => (newA, b))
end pairedAsFreeLeft
