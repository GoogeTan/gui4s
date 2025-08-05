package catnip

import cats.data.{StateT, WriterT}
import cats.{Applicative, ~>}

type Set[F[_], T] = T => F[Unit]

object Set:
  def stateT[F[_] : Applicative, S] : Set[StateT[F, S, *], S] =
    StateT.set
  end stateT

  def writerT[F[_] : Applicative, S] : Set[WriterT[F, S, *], S] =
    WriterT.tell
  end writerT

  def mapK[F[_], G[_], T](intitial : Set[F, T])(f : F ~> G) : Set[G, T] =
    value => f(intitial(value))
  end mapK

  def contramap[F[_], N, M](origianl : Set[F, N])(f : M => N) : Set[F, M] =
    f andThen origianl
  end contramap
end Set
