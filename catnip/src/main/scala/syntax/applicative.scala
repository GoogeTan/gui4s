package catnip
package syntax

import cats.arrow.Strong
import cats.kernel.Monoid
import cats.syntax.all.*
import cats.{Applicative, ApplicativeError, Functor}

import scala.Tuple.:*

object applicative:
  given applicativesAreMonoids[F[_] : Applicative as A] : Monoid[F[Unit]] with
    override def empty: F[Unit] =
      A.pure(())
    end empty

    override def combine(x: F[Unit], y: F[Unit]): F[Unit] =
      x *> y
    end combine
  end applicativesAreMonoids
  
  given nestedFunctorsAreFunctors[F[_] : Functor, G[_] : Functor] : Functor[[Value] =>> F[G[Value]]] with
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      fa.map(_.map(f))
    end map
  end nestedFunctorsAreFunctors
  
  extension[T](value : Option[T])
    def getOrRaise[F[_], Error](using A : ApplicativeError[F, Error])(error : Error) : F[T] =
      value.map(_.pure[F]).getOrElse(A.raiseError(error))
    end getOrRaise
  end extension

  given[F[_] : Functor] : Strong[[A, B] =>> A => F[B]] with
    override def dimap[A, B, C, D](fab: A => F[B])(f: C => A)(g: B => D): C => F[D] =
      c => fab(f(c)).map(g)
    end dimap

    override def first[A, B, C](fa: A => F[B]): ((A, C)) => F[(B, C)] =
      (a, c) => fa(a).map(b => (b, c))
    end first

    override def second[A, B, C](fa: A => F[B]): ((C, A)) => F[(C, B)] =
      (c, a) => fa(a).map(b => (c, b))
    end second
  end given
  
  extension[T](value : Option[T])
    def getOrRaiseError[F[_], Error](using AE: ApplicativeError[F, Error])(error : => Error) : F[T] =
      value.map(_.pure[F]).getOrElse(AE.raiseError(error))
    end getOrRaiseError
  end extension
end applicative
