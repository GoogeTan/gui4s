package catnip
package syntax

import catnip.resource.EvalC
import cats.FlatMap
import cats.syntax.all.*

object resource:
  extension [IO[_], Resource[_] : {FlatMap, EvalC[IO]}, A](value: Resource[A])
    def <*<[B](f: A => IO[B]): Resource[A] =
      value.flatTap(f.andThen(_.eval))
    end <*<
  end extension
end resource
