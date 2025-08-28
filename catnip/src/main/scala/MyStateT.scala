package catnip

import cats.arrow.FunctionK
import cats.{Applicative, Functor, Monad, MonadError, ~>}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*

final case class MyStateT[F[_], S, A](run: S => F[(S, A)]):
  def map[B](f: A => B)(using F: Functor[F]): MyStateT[F, S, B] =
    MyStateT(s => F.map(run(s))((s2, a) => (s2, f(a))))
  end map

  def flatMap[B](f: A => MyStateT[F, S, B])(using F: Monad[F]): MyStateT[F, S, B] =
    MyStateT(s =>
      F.flatMap(run(s))((s2, a) =>
        f(a).run(s2)
      )
    )
  end flatMap

  def eval(initial: S)(using F: Functor[F]): F[A] =
    F.map(run(initial))(_._2)
  end eval

  def exec(initial: S)(using F: Functor[F]): F[S] =
    F.map(run(initial))(_._1)
  end exec


end MyStateT

object MyStateT:
  def pure[F[_] : Applicative, S, A](a: A): MyStateT[F, S, A] =
    MyStateT(s => (s, a).pure)
  end pure

  def get[F[_] : Applicative, S]: MyStateT[F, S, S] =
    MyStateT(s => (s, s).pure)
  end get

  def set[F[_] : Applicative, S](s: S): MyStateT[F, S, Unit] =
    MyStateT(_ => (s, ()).pure)
  end set

  def modify[F[_] : Applicative, S](f: S => S): MyStateT[F, S, Unit] =
    MyStateT(s => (f(s), ()).pure)
  end modify

  def liftF[F[_] : Functor, S, T](f : F[T]) : MyStateT[F, S, T] =
    liftK(f)
  end liftF
  
  def liftK[F[_] : Functor, S]: F ~> MyStateT[F, S, *] =
    new FunctionK[F, MyStateT[F, S, *]]:
      def apply[A](fa: F[A]): MyStateT[F, S, A] =
        MyStateT(s => fa.map(a => (s, a)))
    end new
  end liftK

  def liftFunctionK[F[_], G[_], S](fk: F ~> G): MyStateT[F, S, *] ~> MyStateT[G, S, *] =
    new FunctionK[MyStateT[F, S, *], MyStateT[G, S, *]]:
      def apply[A](fa: MyStateT[F, S, A]): MyStateT[G, S, A] =
        MyStateT(s => fk(fa.run(s)))
    end new
  end liftFunctionK

  given[IO[_], State, Error](using M: MonadError[IO, Error]) : MonadError[MyStateT[IO, State, *], Error] with
    val original : Monad[MyStateT[IO, State, *]] = catnip.syntax.transformer.stateTInstance[State].monadInstance[IO]
    export original.*

    override def raiseError[A](e: Error): MyStateT[IO, State, A] =
      MyStateT(_ => M.raiseError(e))
    end raiseError

    override def handleErrorWith[A](fa: MyStateT[IO, State, A])(f: Error => MyStateT[IO, State, A]): MyStateT[IO, State, A] = 
      MyStateT(state =>
        M.handleErrorWith(
         fa.run(state) 
        )(
          error => f(error).run(state)
        )
      )
  end given
end MyStateT
