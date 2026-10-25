package catnip
package syntax

import cats.Id
import cats.data.NonEmptyList

object zip:
  given Zip[Id] with
    override def zip[A, B](a: Id[A], b: Id[B]): (A, B) =
      (a, b)
    end zip
  end given
  
  given Zip[List] with
    override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] =
      a.zip(b)
    end zip
  end given
  
  given Zip[NonEmptyList] with
    override def zip[A, B](a: NonEmptyList[A], b: NonEmptyList[B]): NonEmptyList[(A, B)] =
      a.zip(b)
    end zip
  end given
end zip 

  
  
