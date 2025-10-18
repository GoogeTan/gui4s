package gui4s.desktop.kit.common

import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import cats.*

trait ResourceCell[IO[_], T]:
  def get : IO[T]
  def evalUpdate(f : T => Resource[IO, T]) : IO[Unit]

  def eval[G[_], B](f : T => G[B], g : IO ~> G)(using MonadCancel[G, Throwable]) : G[B]
end ResourceCell

object ResourceCell:
  def atomic[IO[_] : Concurrent, T](initial : Resource[IO, T]) : Resource[IO, ResourceCell[IO, T]] =
    Resource(
      for
        initialValue <- initial.allocated
        ref : Ref[IO, (T, IO[Unit])] <- Ref[IO].of(initialValue)
        mutex <- Mutex[IO]
      yield (
        new ResourceCell[IO, T] {
          override def get: IO[T] =
            mutex.lock.use(_ =>
              ref.get.map(_._1)
            )
          end get

          override def evalUpdate(f: T => Resource[IO, T]): IO[Unit] =
            mutex.lock.use(_ =>
              ref.get.flatMap((value, destructor) =>
                f(value).allocated <* destructor
              ).flatMap(ref.set)
            )
          end evalUpdate

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
  end atomic
end ResourceCell