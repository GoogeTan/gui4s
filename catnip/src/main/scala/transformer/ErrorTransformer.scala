package catnip.transformer

import catnip.syntax.transformer.<>
import cats.Monad
import cats.data.EitherT

type ErrorTransformer[Error] = [IO[_], T] =>> EitherT[IO, Error, T]

object ErrorTransformer:
  def raiseError_[IO[_] : Monad, Error, T](error : Error) : EitherT[IO, Error, T] =
    EitherT.leftT(error)
  end raiseError_

  def raiseError[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, Error, T](error : Error) : (F <> ErrorTransformer[Error])[IO, T] =
    FMT.liftK[EitherT[IO, Error, *]](raiseError_(error))
  end raiseError
end ErrorTransformer
