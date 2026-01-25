package gui4s.core.kit
package effects

import catnip.Get
import catnip.Set
import catnip.syntax.transformer.{_, given}
import catnip.transformer._
import cats._
import cats.data._

import gui4s.core.widget.Path

type PlacementEffectMonadTransformer[Bounds, Error] = ReaderTransformer[Path] <> StateTransformer[Bounds] <> ErrorTransformer[Error]

given[Bounds, Error]: MonadTransformer[PlacementEffectMonadTransformer[Bounds, Error]] =
  composedMonadTransformerInstance[ReaderTransformer[Path] <> StateTransformer[Bounds], ErrorTransformer[Error]]

type PlacementEffect[IO[_], Bounds, Error, Value] = PlacementEffectMonadTransformer[Bounds, Error][IO, Value]

type PlacementEffectC[IO[_], Bounds, Error] = PlacementEffect[IO, Bounds, Error, *]

object PlacementEffect:
  given monadInstance[IO[_] : Monad, Bounds, Error] : MonadError[PlacementEffectC[IO, Bounds, Error], Error] =
    summon

  def liftK[IO[_] : Monad, Bounds, Error] : IO ~> PlacementEffectC[IO, Bounds, Error] =
    MonadTransformer[PlacementEffectMonadTransformer[Bounds, Error]].liftK
  end liftK

  def liftF[IO[_] : Monad, Bounds, Error, Value](value : IO[Value]) : PlacementEffect[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad, Bounds, Error]: Get[PlacementEffect[IO, Bounds, Error, *], Bounds] =
    StateTransformer.get
  end getBounds

  def setBounds[IO[_] : Monad, Bounds, Error]: Set[PlacementEffect[IO, Bounds, Error, *], Bounds] =
    StateTransformer.set
  end setBounds
  
  def withBounds[IO[_] : Monad, Bounds, Error, T](original : PlacementEffect[IO, Bounds, Error, T], f : Bounds => Bounds) : PlacementEffect[IO, Bounds, Error, T] =
    StateTransformer.modifyScoped(original, f)
  end withBounds

  def raiseError[IO[_] : Monad, Bounds, Error, Value](error : => Error) : PlacementEffect[IO, Bounds, Error, Value] =
    ErrorTransformer.raiseError[ReaderTransformer[Path] <> StateTransformer[Bounds], IO, Error, Value](error)
  end raiseError

  def run[IO[_] : Monad, Bounds, Error](path : Path, bounds : IO[Bounds]) : PlacementEffectC[IO, Bounds, Error] ~> EitherT[IO, Error, *] =
    new ~>[
      PlacementEffectC[IO, Bounds, Error], EitherT[IO, Error, *]
    ]:
      override def apply[A](fa: PlacementEffectC[IO, Bounds, Error][A]): EitherT[IO, Error, A] =
        EitherT.liftF(bounds).flatMap(fa.run(path).runA)
      end apply
    end new
  end run

  def currentPath[IO[_] : Monad, Bounds, Error] : PlacementEffect[IO, Bounds, Error, Path] =
    ReaderTransformer.ask_
  end currentPath

  def addNameToPath[IO[_] : Monad, Bounds, Error](name : String)
    : PlacementEffectC[IO, Bounds, Error] ~> PlacementEffectC[IO, Bounds, Error] =
      new ~>[PlacementEffectC[IO, Bounds, Error], PlacementEffectC[IO, Bounds, Error]]:
        override def apply[A](fa: PlacementEffectC[IO, Bounds, Error][A]): PlacementEffectC[IO, Bounds, Error][A] =
          currentPath[IO, Bounds, Error].map(path => path / name).flatMap(
              path => ReaderTransformer.withValue_(fa, path)
          )
        end apply
      end new
  end addNameToPath
end PlacementEffect

