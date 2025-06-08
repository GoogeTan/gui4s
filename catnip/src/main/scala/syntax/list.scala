package catnip
package syntax

import cats.Functor
import cats.syntax.functor.*

object list:
  /**
   * Производит обработку списка в указаном порядке элементов без нарушенения их порядка в результирующем списке.
   * @param list Список для обработки
   * @param order Порядок обработки 
   * @param f Обработчик списка. Должен вернуть список той же длины, что и исходный
   */
  def orderedListProcessing[F[_] : Functor, A : Ordering, B](list: List[A])(f: List[A] => F[List[B]]): F[List[B]] =
    val indexed = list.zipWithIndex.sortBy((value, _) => value)
    val indexes = indexed.map(_._2)
    val values = indexed.map(_._1)
    f(values).map(res =>
      res.zip(indexes).sortBy(_._2).map(_._1)
    )
  end orderedListProcessing
end list

  
