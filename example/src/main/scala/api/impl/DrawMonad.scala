package me.katze.gui4s.example
package api.impl

type DrawMonadT[MU] = [F[_]] =>> DrawMonad[F, MU]

trait DrawMonad[F[_], MU]:
  def move[T](dx : MU, dy : MU, effect : F[T]) : F[T]
end DrawMonad
