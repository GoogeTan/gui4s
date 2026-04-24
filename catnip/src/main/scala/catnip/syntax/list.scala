package catnip
package syntax

import catnip.Zip.zip
import cats.Applicative
import cats.Foldable
import cats.Functor
import cats.Id
import cats.Monad
import cats.Monoid
import cats.Order
import cats.SemigroupK
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.StateT
import cats.syntax.all.*

object list:
  

  def fromList[C[_] : {Applicative as A, SemigroupK as S}, T](c : NonEmptyList[T]) : C[T] =
    c.reduceMapK(A.pure)
  end fromList

  type TraverseOrdered[Effect[_], Collection[_], A] =
    [B] => Collection[A]  => (Collection[A] => Effect[Collection[B]])=> Effect[Collection[B]]

  /**
   * Производит обработку списка в указаном порядке элементов без нарушенения их порядка в результирующем списке.
   */
  def traverseOrdered[F[_] : Functor, C[_] : {Traverse as CT, Sortable as CS, Zip}, A : Order] : TraverseOrdered[F, C, A] =
    [B] => list => f =>
      val sorted = CS.sortBy(
        CT.zipWithIndex(list),
        _._1
      )
      val (sortedValues, indexes) = sorted.unzip
      //TODO мы помним перестановку: можно за линейное время получить ответ.
      f(sortedValues).map(res => CS.sortBy(res.zip(indexes), _._2).map(_._1))
  end traverseOrdered

  def traverseUnordered[F[_] : Functor, C[_] : {Traverse as CT}, A] : TraverseOrdered[F, C, A] =
    [B] => list => f => f(list)
  end traverseUnordered

  def traverseOne[F[_], A]: TraverseOrdered[F, Id, A] =
    [B] => value => f => f(value)
  end traverseOne

  def foldOrdered[M : Monoid, C[_] : {Sortable as CS, Foldable}] :
    [A : Order] => C[A] => (A => M) => M =
     [A : Order] => list => f =>
        CS.sort(list).foldMap(f)
  end foldOrdered
  
  def traverseUntil[F[_] : Monad, G[_] : Traverse, A, B](original : G[A], main : A => F[(B, Boolean)], afterAll : A => F[B]) : F[G[B]] =
    original.traverse[[Value] =>> StateT[F, Boolean, Value], B](
      element =>
        StateT(
          shouldStop =>
            if shouldStop then
              afterAll(element).map(a => (true, a))
            else
              main(element).map((a, b) => (b, a))
        )
    ).runA(false)
  end traverseUntil

  extension[A](value: A)
    def one: List[A] =
      List(value)
    end one
  end extension

  def traverseState[Collection[_] : Traverse, F[_] : Monad, A, B, C](a: Collection[A], f: (A, C) => F[(B, C)], initial: C): F[(Collection[B], C)] =
    val liftedF: A => StateT[F, C, B] = item => StateT(s => f(item, s).map((a, b) => (b, a)))

    a.traverse(liftedF).run(initial).map((a, b) => (b, a))
  end traverseState
end list

  
