package catnip.transformer

import catnip.syntax.transformer.<>
import cats.Monad
import cats.data.OptionT

type OptionTransformer[IO[_], T] = OptionT[IO, T]

object OptionTransformer:
  def none_[IO[_] : Monad, T] : OptionTransformer[IO, T] =
    OptionT.none
  end none_

  def none[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, T] : (F <> OptionTransformer)[IO, T] =
    FMT.liftK[OptionT[IO, *]](none_)
  end none
end OptionTransformer
