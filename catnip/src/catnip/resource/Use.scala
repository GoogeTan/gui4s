package catnip
package resource

import cats.effect.Resource
import cats.effect.kernel.MonadCancel

type UseC[IO[_]] = [R[_]] =>> Use[R, IO]

trait Use[Resource[_], IO[_]]:
  def useResource[T, B](value : Resource[T])(f : T => IO[B]) : IO[B]

  extension[T](value : Resource[T])
    def use[B](f : T => IO[B]) : IO[B] =
      useResource(value)(f)
    end use
  end extension
end Use

object Use:
  given[IO[_]](using MonadCancel[IO, Throwable]) : Use[Resource[IO, *], IO] with
    override def useResource[T, B](value: Resource[IO, T])(f: T => IO[B]): IO[B] =
      value.use(f)
    end useResource
  end given
end Use
    