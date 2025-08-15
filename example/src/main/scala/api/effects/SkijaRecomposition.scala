package me.katze.gui4s.example
package api.effects

import cats.*
import cats.syntax.all.*
import catnip.syntax.all.{*, given}

opaque type SkijaRecomposition[F[_]] = F[Unit]

object SkijaRecomposition:
  def empty[F[_] : Applicative] : SkijaRecomposition[F] = ().pure[F]

  def run[F[_]](recomposition : SkijaRecomposition[F]) : F[Unit] = recomposition

  def lift[F[_] : Functor, T](value : F[T]) : SkijaRecomposition[F] = value.as(())
  
  given[F[_] : Applicative]: Monoid[SkijaRecomposition[F]] = summon