package me.katze.gui4s.example
package application

type Run_[G[_]] = [F[_]] =>> Run[F, G]

trait Run[F[_], G[_]]:
  def run[T](value : F[T]) : G[T]
end Run


def run[F[_], G[_], T](value: F[T])(using r : Run[F, G]): G[T] =
  r.run(value)
end run
