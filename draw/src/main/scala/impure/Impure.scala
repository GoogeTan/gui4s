package me.katze.gui4s.draw
package impure

trait Impure[+F[_]]:
  def impure[A](trunk : => A) : F[A]
end Impure

