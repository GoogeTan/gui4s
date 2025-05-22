package catnip

trait BiMonad[F[_, +_]]:
  def flatMapFirst[A, B, C](value : F[A, B])(f: A => F[C, B]): F[C, B]
  def tailRecM[A, B, E](a: A)(f: A => F[Either[A, B], E]): F[B, E]
  def pure[A](value : A) : F[A, Nothing]
  def mapSecond[A, B, D](value : F[A, B])(f : B => D) : F[A, D] 
end BiMonad

object BiMonad:
  def apply[F[+_, +_]](using a : BiMonad[F]): BiMonad[F] = a
end BiMonad

