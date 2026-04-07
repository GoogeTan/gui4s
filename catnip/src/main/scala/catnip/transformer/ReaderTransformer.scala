package catnip.transformer

import catnip.syntax.transformer.<>
import cats.arrow.FunctionK
import cats.{Applicative, Functor, Monad, ~>}
import cats.data.*

type ReaderTransformer[Context] = [IO[_], T] =>> ReaderT[IO, Context, T]

object ReaderTransformer:
  def ask_[IO[_] : Applicative, Context] : ReaderT[IO, Context, Context] =
    ReaderT.ask[IO, Context]
  end ask_

  def ask[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, Context] : (F <> ReaderTransformer[Context])[IO, Context] =
    FMT.liftK[ReaderT[IO, Context, *]](ask_)
  end ask

  def withValue_[IO[_], C1, C2, T](original : ReaderT[IO, C2, T], f : C1 => C2) : ReaderT[IO, C1, T] =
    ReaderT[IO, C1, T](value =>
      original.run(f(value))
    )
  end withValue_

  def withValueK_[IO[_], C1, C2](f: C1 => C2): ReaderT[IO, C2, *] ~> ReaderT[IO, C1, *] =
    new (ReaderT[IO, C2, *] ~> ReaderT[IO, C1, *]):
      override def apply[A](fa: ReaderT[IO, C2, A]): ReaderT[IO, C1, A] =
        withValue_(fa, f)
      end apply
    end new
  end withValueK_

  def withValue[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    C1,
    C2,
    T
  ](
     original :  (F <> ReaderTransformer[C2])[IO, T],
     f :  C1 => C2,
  ) :  (F <> ReaderTransformer[C1])[IO, T] =
    FMT.innerTransform(
      original,
      [Inner[_] : Functor] => (readerOriginal : ReaderT[IO, C2, Inner[T]]) =>
        withValue_[IO, C1, C2, Inner[T]](readerOriginal, f)
    )
  end withValue

  def withValueK[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    C1,
    C2,
    T
  ](
    f: C1 => C2,
  ): (F <> ReaderTransformer[C2])[IO, *] ~> (F <> ReaderTransformer[C1])[IO, *] =
    new ((F <> ReaderTransformer[C2])[IO, *] ~> (F <> ReaderTransformer[C1])[IO, *]):
      override def apply[A](fa: (F <> ReaderTransformer[C2])[IO, A]): (F <> ReaderTransformer[C1])[IO, A] =
        withValue(fa, f)
      end apply
    end new
  end withValueK
end ReaderTransformer
