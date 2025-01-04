package me.katze.gui4s.draw
package impure

trait ImpureError[F[_], Error]:
  def impureTry[T](from : => Either[Error, T]) : F[T] 
end ImpureError

