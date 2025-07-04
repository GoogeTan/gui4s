package catnip
package syntax

import cats.Monad
import cats.syntax.all.*

object monad:
  given [Ctx, F[_] : Monad as M] : Monad[[Value] =>> Ctx => F[Value]] with
    override def pure[B](x: B): Ctx => F[B] =
      _ => x.pure[F]
    end pure

    override def flatMap[A, B](fa: Ctx => F[A])(f: A => Ctx => F[B]): Ctx => F[B] =
      ctx => fa(ctx).flatMap(f(_)(ctx))
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => Ctx => F[Either[A, B]]): Ctx => F[B] =
      ctx => M.tailRecM(a)(f(_)(ctx))
    end tailRecM
  end given
end monad
