package catnip.transformer

import catnip.syntax.transformer.<>
import cats.{Monad, MonadError, Applicative}
import cats.data.EitherT
import cats.syntax.all.*

type ErrorTransformer[Error] = [IO[_], T] =>> EitherT[IO, Error, T]

object ErrorTransformer:
  def raiseError_[IO[_] : Monad, Error, T](error : Error) : EitherT[IO, Error, T] =
    EitherT.leftT(error)
  end raiseError_

  def raiseError[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, Error, T](error : Error) : (F <> ErrorTransformer[Error])[IO, T] =
    FMT.liftK[EitherT[IO, Error, *]](raiseError_(error))
  end raiseError

  given monadErrorInstance[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad as IOM, Error]: MonadError[(F <> ErrorTransformer[Error])[IO, *], Error] with
    val original : Monad[(F <> ErrorTransformer[Error])[IO, *]] = FMT.monadInstance[EitherT[IO, Error, *]]
    export original.*
    
    override def raiseError[A](e: Error): (F <> ErrorTransformer[Error])[IO, A] =
      FMT.liftK[EitherT[IO, Error, *]](EitherT.left(e.pure))
    end raiseError

    override def handleErrorWith[A](fa: (F <> ErrorTransformer[Error])[IO, A])(f: Error => (F <> ErrorTransformer[Error])[IO, A]): (F <> ErrorTransformer[Error])[IO, A] =
      FMT.monadInstance[EitherT[IO, Error, *]].flatten(
        FMT.innerTransform[EitherT[IO, Error, *], EitherT[IO, Error, *], A, (F <> ErrorTransformer[Error])[IO, A]](fa, [Inner[_] : Applicative] => eitherT =>
          EitherT.right(
            IOM.map(
              eitherT.value
            ) {
              case Right(value) => value.map(_.pure)
              case Left(error : Error) => f(error).pure
            }
          )
        )
      ) 
  end monadErrorInstance
end ErrorTransformer
