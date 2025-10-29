package catnip

import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*

trait ResourceCell[IO[_], T]:
  def evalReplace(f : T => Resource[IO, T]) : IO[Unit]

  def get : Resource[IO, T]
end ResourceCell

object ResourceCell:
  def blocking[
    IO[_] : Concurrent,
    T
  ](
     initial : Resource[IO, T],
   ) : Resource[IO, ResourceCell[IO, T]] =
    Resource(
      for
        initialValue <- initial.allocated
        ref : Ref[IO, (T, IO[Unit])] <- Ref[IO].of(initialValue)
        mutex <- Mutex[IO]
      yield (
        new ResourceCell[IO, T] {
          override def evalReplace(f: T => Resource[IO, T]): IO[Unit] =
            mutex.lock.use(_ =>
              ref.get.flatMap((value, destructor) =>
                f(value).allocated <* destructor
              ).flatMap(ref.set)
            )
          end evalReplace

          override def get: Resource[IO, T] =
            mutex.lock.evalMap(_ =>
              ref.get.map(_._1)
            )
          end get
        },
        mutex.lock.use(_ =>
          ref.get.flatMap(_._2)
        )
      )
    )
  end blocking
end ResourceCell