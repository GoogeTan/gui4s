package catnip.transformer

import catnip.syntax.transformer.{*, given}
import cats.data.StateT
import cats.{Applicative, Functor, Monad}
import cats.syntax.all.*

type StateTransformer[State] = [IO[_], T] =>> StateT[IO, State, T]

object StateTransformer:
  def get_[IO[_] : Applicative, State] : StateT[IO, State, State] =
    StateT.get
  end get_

  def get[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State] : (F <> StateTransformer[State])[IO, State] =
    FMT.liftK[StateT[IO, State, *]](get_)
  end get

  def set_[IO[_] : Applicative, State](coordinates: State): StateT[IO, State, Unit] =
    StateT.set(coordinates)
  end set_

  def set[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State](coordinates: State): (F <> StateTransformer[State])[IO, Unit] =
    FMT.liftK[StateT[IO, State, *]](set_(coordinates))
  end set

  def modify_[IO[_] : Applicative, State](f: State => State): StateT[IO, State, Unit] =
    StateT.modify(f)
  end modify_

  def modify[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State](f: State => State): (F <> StateTransformer[State])[IO, Unit] =
    FMT.liftK[StateT[IO, State, *]](modify_(f))
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
      originalState <- get[F, IO, State]
      _ <- set(f(originalState))
      res <- original
      _ <- set(originalState)
    yield res
  end modifyScoped

  def run[F[_[_], _] : {MonadTransformer as FMT}, IO[_] : Monad, State, T](original : (F <> StateTransformer[State])[IO, T], initialState: State) : F[IO, (T, State)] =
    FMT.innerTransform[StateT[IO, State, *], IO, T, (T, State)](
      original,
      [Inner[_] : Functor] => value =>
        value.run(initialState).map((state, innerT) =>
          innerT.map(t => (t, state))
        )
    )
end StateTransformer

