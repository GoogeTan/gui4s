package catnip
package syntax

import cats.Monad
import cats.syntax.all.*

object state:
  given stateTMonad[F[_]: Monad, S]: Monad[MyStateT[F, S, *]] with
    def pure[A](a: A): MyStateT[F, S, A] =
      MyStateT.pure(a)
    end pure

    def flatMap[A, B](fa: MyStateT[F, S, A])(f: A => MyStateT[F, S, B]): MyStateT[F, S, B] =
      fa.flatMap(f)
    end flatMap

    def tailRecM[A, B](a: A)(f: A => MyStateT[F, S, Either[A, B]]): MyStateT[F, S, B] =
      MyStateT(s =>
        Monad[F].tailRecM((s, a))(
          (s1, a1) =>
            f(a1).run(s1).map {
              case (s2, Left(a2)) => Left[(S, A), (S, B)]((s2, a2))
              case (s2, Right(b)) => Right[(S, A), (S, B)]((s2, b))
            }
        )
      )
    end tailRecM
  end stateTMonad
end state
