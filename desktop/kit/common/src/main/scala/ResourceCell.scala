package gui4s.desktop.kit.common

import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import cats.*


trait ResourceCell[IO[_], Resource[_], T]:
  def get : IO[T]
  def evalUpdate(f : T => Resource[T]) : IO[Unit]

  def eval[G[_], B](f : T => G[B], g : IO ~> G)(using MonadCancel[G, Throwable]) : G[B]
end ResourceCell

object ResourceCell:
  def atomic[IO[_] : Concurrent, Resource[_], T](
                                                  initial : Resource[T],
                                                  runResource : Resource[T] => IO[(T, IO[Unit])],
                                                  makeResource : [U] => IO[(U, IO[Unit])] => Resource[U]
                                                ) : Resource[ResourceCell[IO, Resource, T]] =
    makeResource(
      for
        ref : Ref[IO, (T, IO[Unit])] <- Ref[IO].of(runResource(initial))
        mutex <- Mutex[IO]
      yield (
        new ResourceCell[IO, Resource, T] {
          override def get: IO[T] =
            mutex.lock.use(_ =>
              ref.get.map(_._1)
            )
          end get

          override def evalUpdate(f: T => Resource[T]): IO[Unit] =
            mutex.lock.use(_ =>
              ref.get.flatMap((value, destructor) =>
                runResource(f(value)) <* destructor
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