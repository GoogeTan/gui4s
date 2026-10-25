package catnip.transformer

import cats.arrow.FunctionK
import cats.{Applicative, Functor, Monad, ~>}
import cats.syntax.all.*
import catnip.Patch

final case class IncrementalStateT[F[_], G[_], State, T](value : State => F[(G[State], T)]):
  def bimap[NewState](
    f : NewState => State,
    g : (NewState, State) => G[NewState]
  )(using F : Functor[F], patch : Patch[G]) : IncrementalStateT[F, G, NewState, T] =
    IncrementalStateT(
      originalNewState =>
        val originalState = f(originalNewState)
        value(originalState).map(
          (st, t) => 
            (
              g(originalNewState, patch.applyTo(originalState, st)), 
              t
            )
        )
    )
  end bimap

  def runS(state : State)(using F: Functor[F]) : F[G[State]] =
    F.map(
      value(state)
    )(_._1)
  end runS
end IncrementalStateT

object IncrementalStateT:
  def modify[F[_] : Functor, G[_], State](f : State => F[G[State]]) : IncrementalStateT[F, G, State, Unit] =
    IncrementalStateT(s => f(s).map((_, ())))
  end modify

  def eval[F[_] : Functor, G[_] : Patch as INC, State, Value](f : State => F[Value]) : IncrementalStateT[F, G, State, Value] =
    IncrementalStateT(s => f(s).map((INC.empty, _)))
  end eval

  def get[F[_] : Applicative as A, G[_] : Patch as INC, State] : IncrementalStateT[F, G, State, State] =
    IncrementalStateT(
       st =>
         A.pure((INC.empty, st))
    )
  end get

  //TODO relax requirements for FlatMap and applicative instances
  given monadInstance[F[_] : Monad as FM, G[_] : Patch as INC, State] : Monad[IncrementalStateT[F, G, State, *]] with
    override def pure[A](x: A): IncrementalStateT[F, G, State, A] =
      IncrementalStateT(_ => FM.pure((INC.empty, x)))
    end pure

    override def flatMap[A, B](
      fa: IncrementalStateT[F, G, State, A]
    )(
      f: A => IncrementalStateT[F, G, State, B]
    ): IncrementalStateT[F, G, State, B] =
      IncrementalStateT(
        initialState =>
          FM.flatMap(fa.value(initialState)):
            (newStateOpt, a) =>
              val newState = INC.applyTo(initialState, newStateOpt)
              FM.map(f(a).value(newState)):
                (newestStateOpt, b) =>
                  (INC.merge(newStateOpt, newestStateOpt), b)
      )
    end flatMap

    override def tailRecM[A, B](a: A)(f: A => IncrementalStateT[F, G, State, Either[A, B]]): IncrementalStateT[F, G, State, B] =
      IncrementalStateT(
        initialState =>
          FM.tailRecM[(State, G[State], A), (G[State], B)]((initialState, INC.empty, a)) {
            case (currentState, acc, currentA) =>
              FM.map(f(currentA).value(currentState)) {
                case (newStateOpt, res) =>
                  val combined = INC.merge(acc, newStateOpt)
                  res match {
                    case Left(nextA) =>
                      Left((INC.applyTo(currentState, newStateOpt), combined, nextA))
                    case Right(b) =>
                      Right((combined, b))
                  }
              }
          }
      )
    end tailRecM
  end monadInstance

  given monadTransformerInstance[G[_] : Patch, State] : MonadTransformer[[F[_], T] =>> IncrementalStateT[F, G, State, T]] with
    override def monadInstance[IO[_] : Monad]: Monad[IncrementalStateT[IO, G, State, *]] = summon

    override def liftK[H[_] : Monad]: H ~> IncrementalStateT[H, G, State, *] =
      FunctionK.lift(
        [T] => (f : H[T]) =>
          IncrementalStateT(_ => f.map((summon[Patch[G]].empty, _)))
      )
    end liftK

    override def liftFunctionK[H[_] : Monad, K[_] : Monad](f: H ~> K): IncrementalStateT[H, G, State, *] ~> IncrementalStateT[K, G, State, *] =
      FunctionK.lift(
        [T] => (ff : IncrementalStateT[H, G, State, T]) =>
          IncrementalStateT(initialState => f(ff.value(initialState)))
      )
    end liftFunctionK
  end monadTransformerInstance
end IncrementalStateT