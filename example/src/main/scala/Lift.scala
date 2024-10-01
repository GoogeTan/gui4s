package me.katze.gui4s.example

import cats.data.ReaderT

trait Lift[F[_], G[_], T]:
  def lift[L](old : T => F[L]) : G[L]

given liftReadr[F[_], Ctx] : Lift[F, [T] =>> ReaderT[F, Ctx, T], Ctx] with
  override def lift[L](old: Ctx => F[L]): ReaderT[F, Ctx, L] =
    ReaderT.apply[F, Ctx, L](old)
  end lift
end liftReadr
