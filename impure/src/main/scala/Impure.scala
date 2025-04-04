package me.katze.gui4s.impure

trait Impure[+F[_]]:
  def impure[A](trunk : => A) : F[A]
  
  def apply[A](trunk : => A) : F[A] = impure(trunk)
end Impure

