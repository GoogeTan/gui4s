package gui4s.core.kit
package effects

import scala.util.NotGiven

import catnip.Get
import catnip.Set
import catnip.syntax.transformer.{_, given}
import catnip.transformer._
import cats._
import cats.syntax.all._

import gui4s.core.widget.Path

type PlacementEffectMonadTransformer[Bounds] = ReaderTransformer[Path] <> StateTransformer[Bounds]

given[Bounds]: MonadTransformer[PlacementEffectMonadTransformer[Bounds]] =
  composedMonadTransformerInstance[ReaderTransformer[Path], StateTransformer[Bounds]]

type PlacementEffect[IO[_], Bounds, Value] = PlacementEffectMonadTransformer[Bounds][IO, Value]

type PlacementEffectC[IO[_], Bounds] = PlacementEffect[IO, Bounds, *]

object PlacementEffect:
  given monadThrowInstance[IO[_] : MonadThrow, Bounds] : MonadThrow[PlacementEffectC[IO, Bounds]] =
    summon

  given monadInstance[IO[_] : Monad, Bounds](using NotGiven[MonadThrow[IO]]): Monad[PlacementEffectC[IO, Bounds]] =
    summon

  def liftK[IO[_] : Monad, Bounds] : IO ~> PlacementEffectC[IO, Bounds] =
    MonadTransformer[PlacementEffectMonadTransformer[Bounds]].liftK
  end liftK

  def liftF[IO[_] : Monad, Bounds, Value](value : IO[Value]) : PlacementEffect[IO, Bounds, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad, Bounds]: Get[PlacementEffect[IO, Bounds, *], Bounds] =
    StateTransformer.get
  end getBounds

  def setBounds[IO[_] : Monad, Bounds]: Set[PlacementEffect[IO, Bounds, *], Bounds] =
    StateTransformer.set
  end setBounds
  
  def withBounds[IO[_] : Monad, Bounds, T](original : PlacementEffect[IO, Bounds, T], f : Bounds => Bounds) : PlacementEffect[IO, Bounds, T] =
    StateTransformer.modifyScoped(original, f)
  end withBounds

  def run[IO[_] : Monad, Bounds](path : Path, bounds : IO[Bounds]) : PlacementEffectC[IO, Bounds] ~> IO =
    new ~>[
      PlacementEffectC[IO, Bounds], IO
    ]:
      override def apply[A](fa: PlacementEffectC[IO, Bounds][A]): IO[A] =
        bounds.flatMap(fa.run(path).runA)
      end apply
    end new
  end run

  def currentPath[IO[_] : Monad, Bounds] : PlacementEffect[IO, Bounds, Path] =
    ReaderTransformer.ask_
  end currentPath

  def addNameToPath[IO[_] : Monad, Bounds](name : String): PlacementEffectC[IO, Bounds] ~> PlacementEffectC[IO, Bounds] =
      ReaderTransformer.withValueK_[StateTransformer[Bounds][IO, *], Path, Path](_ / name)
  end addNameToPath
end PlacementEffect

