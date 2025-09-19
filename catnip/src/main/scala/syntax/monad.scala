package catnip
package syntax

import cats.syntax.all.*
import cats.{FlatMap, Monad, MonadError}

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

  extension[F[_] : FlatMap as FM, A](value : F[A])
    def >>![B](f : A => F[B]) : F[A] =
      FM.flatTap(value)(f)
    end >>!
  end extension

  extension [F[_] : FlatMap as FM, A](value: Resource[F, A])
    def >>![B](f: A => F[B]): F[A] =
      value.evalTap(f)
    end >>!
  end extension
end monad
