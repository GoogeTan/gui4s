package catnip
package resource

type AllocateC[IO[_]] = [R[_]] =>> Allocate[R, IO]

trait Allocate[Resource[_], IO[_]]:
  def allocate[T](value : Resource[T]) : IO[(T, IO[Unit])]
end Allocate
