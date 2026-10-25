package catnip
package resource

import catnip.transformer.MonadTransformer
import cats.Monad
import cats.~>

type EvalC[IO[_]] = [R[_]] =>> Eval[R, IO]

trait Eval[Resource[_], IO[_]]:
  def liftToResource : IO ~> Resource

  extension[T](value : IO[T])
    def eval : Resource[T] = liftToResource(value)
  end extension
  
  extension[MT[_[_], _] : MonadTransformer as MT, T](value : MT[IO, T])
    def evalK(using Monad[Resource], Monad[IO]) : MT[Resource, T] =
      MT.liftFunctionK(liftToResource)(value)
    end evalK
  end extension
end Eval