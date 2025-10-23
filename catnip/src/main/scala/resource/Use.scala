package catnip
package resource

type UseC[IO[_]] = [R[_]] =>> Use[R, IO]

trait Use[Resource[_], IO[_]]:
  def useResource[T, B](value : Resource[T])(f : T => IO[B]) : IO[B]

  extension[T](value : Resource[T])
    def use[B](f : T => IO[B]) : IO[B] =
      useResource(value)(f)
    end use
  end extension
end Use
