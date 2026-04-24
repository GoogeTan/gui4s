package gui4s.core.kit
package effects

import scala.util.NotGiven

import catnip.Get
import catnip.syntax.transformer.given
import catnip.transformer.*
import cats.*
import cats.syntax.all.*

type PlacementEffectMonadTransformer[Context] = ReaderTransformer[Context]

given[Context]: MonadTransformer[PlacementEffectMonadTransformer[Context]] = summon

type PlacementEffect[IO[_], Context, Value] = PlacementEffectMonadTransformer[Context][IO, Value]

type PlacementEffectC[IO[_], Context] = PlacementEffect[IO, Context, *]

object PlacementEffect:
  given monadThrowInstance[IO[_] : MonadThrow, Context] : MonadThrow[PlacementEffectC[IO, Context]] =
    summon

  given monadInstance[IO[_] : Monad, Context](using NotGiven[MonadThrow[IO]]): Monad[PlacementEffectC[IO, Context]] =
    summon

  def liftK[IO[_] : Monad, Context] : IO ~> PlacementEffectC[IO, Context] =
    MonadTransformer[PlacementEffectMonadTransformer[Context]].liftK
  end liftK

  def liftF[IO[_] : Monad, Context, Value](value : IO[Value]) : PlacementEffect[IO, Context, Value] =
    liftK[IO, Context](value)
  end liftF

  def getContext[IO[_] : Monad, Context]: Get[PlacementEffect[IO, Context, *], Context] =
    ReaderTransformer.ask_
  end getContext
  
  def withContext[IO[_] : Monad, C1, C2, T](original : PlacementEffect[IO, C2, T], f : C1 => C2) : PlacementEffect[IO, C1, T] =
    ReaderTransformer.withValue_(original, f)
  end withContext

  def withContextK[IO[_] : Monad, C1, C2](
                                           f: C1 => C2
                                         ): PlacementEffect[IO, C2, *] ~> PlacementEffect[IO, C1, *] =
    ReaderTransformer.withValueK_(f)
  end withContextK

  def run[IO[_] : Monad, Context](bounds : IO[Context]) : PlacementEffectC[IO, Context] ~> IO =
    new ~>[
      PlacementEffectC[IO, Context], IO
    ]:
      override def apply[A](fa: PlacementEffectC[IO, Context][A]): IO[A] =
        bounds.flatMap(fa.run(_))
      end apply
    end new
  end run
end PlacementEffect

