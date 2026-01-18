package catnip
package syntax

import cats.Applicative
import cats.Foldable
import cats.Functor
import cats.Id
import cats.Monad
import cats.Order
import cats.SemigroupK
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.StateT
import cats.syntax.all._

object list:
  def fromList[C[_] : {Applicative as A, SemigroupK as S}, T](c : NonEmptyList[T]) : C[T] =
    c.reduceMapK(A.pure)
  end fromList

  type TraverseOrdered[Effect[_], Container[_]] =
    [A : Order, B] => Container[A]  => (Container[A] => Effect[Container[B]])=> Effect[Container[B]]

  /**
   * Производит обработку списка в указаном порядке элементов без нарушенения их порядка в результирующем списке.
   */
  def traverseOrdered[F[_] : Functor, C[_] : {Applicative as A, Foldable, SemigroupK}] : TraverseOrdered[F, C] =
    [A : Order, B] => list => f =>
      NonEmptyList.fromList(list.toList) match
        case Some(value) =>
          val indexed = value.zipWithIndex.sortBy((value, _) => value)
          val indexes = indexed.map(_._2)
          val values = fromList(indexed.map(_._1))
          f(values).map(res =>
            fromList(
              NonEmptyList.fromListUnsafe(
                res.toList
              ).zip(indexes).sortBy(_._2).map(_._1)
            )
          )
        case None =>
          f(list)
  end traverseOrdered

  def traverseOne[F[_]] : TraverseOrdered[F, Id] =
      [A : Order, B] => value => f => f(value)
  end traverseOne
  
  def traverseUntil[F[_] : Monad, G[_] : Traverse, A, B](original : G[A], main : A => F[(Boolean, B)], afterAll : A => F[B]) : F[G[B]] =
    original.traverse[[Value] =>> StateT[F, Boolean, Value], B](
      element =>
        StateT(
          shouldStop =>
            if shouldStop then
              afterAll(element).map(a => (true, a))
            else
              main(element)
        )
    ).runA(false)
  end traverseUntil

  extension[A](value: A)
    def one: List[A] =
      List(value)
    end one
  end extension
end list

  
