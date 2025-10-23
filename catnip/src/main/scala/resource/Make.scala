package catnip
package resource

type MakeC[IO[_]] = [R[_]] =>> Make[R, IO]

trait Make[Resource[_], IO[_]]:
  def make[T](value : IO[(T, IO[Unit])]) : Resource[T]
end Make
