package catnip
package syntax

import cats.{Monad, Monoid}
import cats.syntax.all.*

import scala.annotation.tailrec

object monad:
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
  /*
  given pairMonad[S : Monoid as sm] : Monad[(S, *)] with
    override def pure[A](x: A) : (S, A) =
      (sm.empty, x)
    end pure

    override def flatMap[A, B](fa: (S, A))(f: A => (S, B)) : (S, B) =
      val (s, b) = f(fa._2)
      (fa._1 |+| s, b)
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => (S, Either[A, B])) : (S, B) =
      @tailrec
      def helper(a : A, s : S) : (S, B) =
        f(a) match
          case (ss, Left(aa)) =>
            helper(aa, s |+| ss)
          case (ss, Right(b)) =>
            (s |+| ss, b)
        end match
      end helper

      helper(a, sm.empty)
    end tailRecM
  end pairMonad*/
end monad
