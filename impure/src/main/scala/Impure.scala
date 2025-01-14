package me.katze.gui4s.impure

trait Impure[+F[_]]:
  def impure[A](trunk : => A) : F[A]
end Impure

