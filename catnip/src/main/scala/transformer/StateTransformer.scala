package catnip.transformer

import catnip.syntax.transformer.{*, given}
import catnip.MyStateT
import cats.{Applicative, Monad}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}

given[IO[_] : Monad, State]: Monad[MyStateT[IO, State, *]] = catnip.syntax.state.stateTMonad[IO, State]
type StateTransformer[State] = [IO[_], T] =>> MyStateT[IO, State, T]

object StateTransformer:
  def get_[IO[_] : Applicative, State] : MyStateT[IO, State, State] =
    MyStateT.get
  end get_

  def get[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State] : (F <> StateTransformer[State])[IO, State] =
    FMT.liftK[MyStateT[IO, State, *]](get_)
  end get

  def set_[IO[_] : Applicative, State](coordinates: State): MyStateT[IO, State, Unit] =
    MyStateT.set(coordinates)
  end set_

  def set[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State](coordinates: State): (F <> StateTransformer[State])[IO, Unit] =
    FMT.liftK[MyStateT[IO, State, *]](set_(coordinates))
  end set

  def modify_[IO[_] : Applicative, State](f: State => State): MyStateT[IO, State, Unit] =
    MyStateT.modify(f)
  end modify_

  def modify[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State](f: State => State): (F <> StateTransformer[State])[IO, Unit] =
    FMT.liftK[MyStateT[IO, State, *]](modify_(f))
  end modify


  def modifyScoped_[IO[_] : Monad, State, T](original : StateTransformer[State][IO, T], f: State => State): StateTransformer[State][IO, T] =
    for
      originalState <- get_
      _ <- set_(f(originalState))
      res <- original
      _ <- set_(originalState)
    yield res
  end modifyScoped_
  
  def modifyScoped[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State, T](original : (F <> StateTransformer[State])[IO, T], f: State => State): (F <> StateTransformer[State])[IO, T] =
    for
      originalState <- get
      _ <- set(f(originalState))
      res <- original
      _ <- set(originalState)
    yield res
  end modifyScoped
end StateTransformer

