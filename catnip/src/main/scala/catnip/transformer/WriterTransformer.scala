package catnip.transformer

import catnip.syntax.transformer.<>
import cats.*
import cats.data.*
import cats.syntax.all.*

type WriterTransformer[L] = [IO[_], T] =>> WriterT[IO, L, T]

object WriterTransformer:
  def tell_[IO[_] : Monad, L : Monoid](value : L) : WriterTransformer[L][IO, Unit] =
    WriterT.tell(value)
  end tell_

  def tell[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad, L: Monoid](events : L) : (F <> WriterTransformer[L])[IO, Unit] =
    FMT.liftK[WriterT[IO, L, *]](tell_(events))
  end tell

  def listen_[IO[_] : Monad, L, T](original: WriterT[IO, L, T]): WriterT[IO, L, (T, L)] =
    original.listen
  end listen_

  def listen[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    L: Monoid,
    T
  ](original : (F <> WriterTransformer[L])[IO, T]) : (F <> WriterTransformer[L])[IO, (T, L)] =
    FMT.innerTransform(
      original,
      [Inner[_] : Functor] => (writerOriginal : WriterT[IO, L, Inner[T]]) =>
        listen_(writerOriginal).map((innerT, l) => innerT.map(t => (t, l)))
    )
  end listen

  def drop_[IO[_] : Monad, OldL, NewL: Monoid, T](original: WriterT[IO, OldL, T]): WriterT[IO, NewL, T] =
    WriterT.liftF(original.value)
  end drop_

  def drop[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    OldL : Monoid,
    NewL : Monoid,
    Value
  ](original : (F <> WriterTransformer[OldL])[IO, Value]) : (F <> WriterTransformer[NewL])[IO, Value] =
    FMT.innerTransform(
      original,
      [Inner[_] : Functor] => (writerOriginal : WriterT[IO, OldL, Inner[Value]]) =>
        drop_(writerOriginal)
    )
  end drop

  def extract_[IO[_] : Monad, OldL, NewL: Monoid, T](original: WriterT[IO, OldL, T]): WriterT[IO, NewL, (T, OldL)] =
    drop_(listen_(original))
  end extract_

  def extract[
    F[_[_], _] : MonadTransformer as FMT,
    IO[_] : Monad,
    OldL: Monoid,
    NewL : Monoid,
    Value
  ](original : (F <> WriterTransformer[OldL])[IO, Value]) : (F <> WriterTransformer[NewL])[IO, (Value, OldL)] =
    drop[F, IO, OldL, NewL, (Value, OldL)](listen(original))
  end extract
end WriterTransformer
