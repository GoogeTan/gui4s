package catnip
package syntax

import catnip.CovariantOptionT
import cats.data.OptionT
import cats.syntax.all.*
import cats.{Monad, MonadError}

object monad:
  /**
   * Каррированная версия MonadError.
   */
  type MonadErrorT[T] = [F[_]] =>> MonadError[F, T]

  given readerMonad[Ctx, F[_] : Monad as M] : Monad[[Value] =>> Ctx => F[Value]] with
    override def pure[B](x: B): Ctx => F[B] =
      _ => x.pure[F]
    end pure

    override def flatMap[A, B](fa: Ctx => F[A])(f: A => Ctx => F[B]): Ctx => F[B] =
      ctx => fa(ctx).flatMap(f(_)(ctx))
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => Ctx => F[Either[A, B]]): Ctx => F[B] =
      ctx => M.tailRecM(a)(f(_)(ctx))
    end tailRecM
  end readerMonad
  
  given covariantOptionTMonad[F[+_] : Monad] : Monad[[Value] =>> CovariantOptionT[F, Value]] with
    override def pure[A](x: A): CovariantOptionT[F, A] =
      CovariantOptionT(Some(x).pure[F])
    end pure

    override def flatMap[A, B](fa: CovariantOptionT[F, A])(f: A => CovariantOptionT[F, B]): CovariantOptionT[F, B] =
      CovariantOptionT(
        fa.run.flatMap:
          case Some(value) => f(value).run
          case None => None.pure[F]
      )
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => CovariantOptionT[F, Either[A, B]]): CovariantOptionT[F, B] =
      CovariantOptionT(Monad[OptionT[F, *]].tailRecM(a)(aa => OptionT(f(aa).run)).value)
    end tailRecM
  end covariantOptionTMonad

  given covariantEitherTMonad[F[+_] : Monad, E]: MonadError[[Value] =>> CovariantEitherT[F, E, Value], E] with
    override def pure[A](x: A): CovariantEitherT[F, E, A] =
      CovariantEitherT(Right[E, A](x).pure[F])
    end pure

    override def flatMap[A, B](fa: CovariantEitherT[F, E, A])(f: A => CovariantEitherT[F, E, B]): CovariantEitherT[F, E, B] =
      CovariantEitherT(
        fa.run.flatMap:
          case Right(value) => f(value).run
          case Left(error) => Left[E, B](error).pure[F]
      )
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => CovariantEitherT[F, E, Either[A, B]]): CovariantEitherT[F, E, B] =
      CovariantEitherT(
        Monad[F].tailRecM[A, Either[E, B]](a)(aa =>
          f(aa).run.map:
            case Right(Right(b)) => Right[A, Either[E, B]](Right[E, B](b))
            case Right(Left(nextA)) => Left[A, Either[E, B]](nextA)
            case Left(e) => Right[A, Either[E, B]](Left[E, B](e))
        )
      )
    end tailRecM

    override def raiseError[A](e: E): CovariantEitherT[F, E, A] =
      CovariantEitherT(Left[E, A](e).pure[F])
    end raiseError

    override def handleErrorWith[A](fa: CovariantEitherT[F, E, A])(f: E => CovariantEitherT[F, E, A]): CovariantEitherT[F, E, A] =
      CovariantEitherT(
        fa.run.flatMap:
          case Right(a) => Right[E, A](a).pure[F]
          case Left(e) =>
            f(e).run
      )
  end covariantEitherTMonad

end monad
