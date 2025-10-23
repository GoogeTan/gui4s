package catnip

import catnip.resource.{AllocateC, MakeC}
import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*

trait ResourceCell[IO[_], Resource[_], T]:
  def evalReplace(f : T => Resource[T]) : IO[Unit]

  def eval[G[_], B](f : T => G[B], g : IO ~> G)(using MonadCancel[G, Throwable]) : G[B]
end ResourceCell

object ResourceCell:
  def blocking[
    IO[_] : Concurrent,
    Resource[_] : {MakeC[IO] as M, AllocateC[IO] as A},
    T
  ](
     initial : Resource[T],
   ) : Resource[ResourceCell[IO, Resource, T]] =
    M.make(
      for
        initialValue <- A.allocate(initial)
        ref : Ref[IO, (T, IO[Unit])] <- Ref[IO].of(initialValue)
        mutex <- Mutex[IO]
      yield (
        new ResourceCell[IO, Resource, T] {
          override def evalReplace(f: T => Resource[T]): IO[Unit] =
            mutex.lock.use(_ =>
              ref.get.flatMap((value, destructor) =>
                A.allocate(f(value)) <* destructor
              ).flatMap(ref.set)
            )
          end evalReplace

          override def eval[G[_], B](f: T => G[B], g : IO ~> G)(using MonadCancel[G, Throwable]): G[B] =
            mutex.lock.mapK(g).use(_ =>
              g(ref.get).flatMap((value, _) => f(value))
            )
          end eval
        },
        mutex.lock.use(_ =>
          ref.get.flatMap(_._2)
        )
      )
    )
  end blocking
end ResourceCell