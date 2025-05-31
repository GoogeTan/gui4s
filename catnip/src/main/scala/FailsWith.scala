package catnip

trait FailsWith[F[_, _]]:
  def failWith[T, E](e: E): F[E, T]
  def mapError[T, E1, E2](fa: F[E1, T])(f: E1 => E2): F[E2, T]
end FailsWith

