package catnip
package resource

import cats.~>

type EvalC[IO[_]] = [R[_]] =>> Eval[R, IO]

trait Eval[Resource[_], IO[_]]:
  def evalK : IO ~> Resource

  extension[T](value : IO[T])
    def eval : Resource[T] = evalK(value)
  end extension
end Eval