package me.katze.gui4s.impure

trait Impure[+F[_]]:
  /**
   * Позволяет оборачивать грязное вычисление в контейнер(монаду).
   * @param trunk вычисление
   * @tparam A тип результата
   * @return Ссылочно прозрачный контейнер для результата
   */
  def impure[A](trunk : => A) : F[A]
  
  def apply[A](trunk : => A) : F[A] = impure(trunk)
end Impure

