package catnip.transformer

import catnip.syntax.transformer.<>
import cats.data.*
import cats.{Applicative, Functor, Monad}

type ReaderTransformer[Context] = [IO[_], T] =>> ReaderT[IO, Context, T]

object ReaderTransformer:
  def ask_[IO[_] : Applicative, Context] : ReaderT[IO, Context, Context] =
    ReaderT.ask[IO, Context]
  end ask_

  def ask[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, Context] : (F <> ReaderTransformer[Context])[IO, Context] =
    FMT.liftK[ReaderT[IO, Context, *]](ask_)
  end ask

  def withValue_[IO[_], Context, T](original : ReaderT[IO, Context, T], value : Context) : ReaderT[IO, Context, T] =
    ReaderT.liftF[IO, Context, T](
      original.run(value)
    )
  end withValue_

  def withValue[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    Context,
    T
  ](
     original :  (F <> ReaderTransformer[Context])[IO, T],
     value : Context,
  ) :  (F <> ReaderTransformer[Context])[IO, T] =
    FMT.innerTransform(
      original,
      [Inner[_] : Functor] => (readerOriginal : ReaderT[IO, Context, Inner[T]]) =>
        withValue_[IO, Context, Inner[T]](readerOriginal, value)
    )
  end withValue
end ReaderTransformer
