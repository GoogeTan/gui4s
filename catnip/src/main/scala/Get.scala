package catnip

import cats.{Applicative, ~>}
import cats.data.{ReaderT, StateT}

type Get[F[_], T] = F[T]

object Get:
  def stateT[F[_] : Applicative, S] : Get[StateT[F, S, *], S] =
    StateT.get
  end stateT

  def readerT[F[_] : Applicative, S] : Get[ReaderT[F, S, *], S] =
    ReaderT.ask
  end readerT

  def mapK[F[_], G[_], T](initial : Get[F, T])(f : F ~> G) : Get[G, T] =
    f(initial)
  end mapK
end Get
