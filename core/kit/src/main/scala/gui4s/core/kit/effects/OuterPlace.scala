package gui4s.core.kit
package effects

import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import catnip.{Get, Set}
import cats.*
import cats.data.*
import gui4s.core.widget.Path

type OuterPlaceTransformer[Bounds, Error] = ReaderTransformer[Path] <> StateTransformer[Bounds] <> ErrorTransformer[Error]

given[Bounds, Error]: MonadTransformer[OuterPlaceTransformer[Bounds, Error]] =
  composedMonadTransformerInstance[ReaderTransformer[Path] <> StateTransformer[Bounds], ErrorTransformer[Error]]

type OuterPlace[IO[_], Bounds, Error, Value] = OuterPlaceTransformer[Bounds, Error][IO, Value]

type OuterPlaceC[IO[_], Bounds, Error] = OuterPlace[IO, Bounds, Error, *]

object OuterPlace:
  given monadInstance[IO[_] : Monad, Bounds, Error] : MonadError[OuterPlaceC[IO, Bounds, Error], Error] =
    summon

  def liftK[IO[_] : Monad, Bounds, Error] : IO ~> OuterPlaceC[IO, Bounds, Error] =
    MonadTransformer[OuterPlaceTransformer[Bounds, Error]].liftK
  end liftK

  def liftF[IO[_] : Monad, Bounds, Error, Value](value : IO[Value]) : OuterPlace[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad, Bounds, Error]: Get[OuterPlace[IO, Bounds, Error, *], Bounds] =
    StateTransformer.get
  end getBounds

  def setBounds[IO[_] : Monad, Bounds, Error]: Set[OuterPlace[IO, Bounds, Error, *], Bounds] =
    StateTransformer.set
  end setBounds
  
  def withBounds[IO[_] : Monad, Bounds, Error, T](original : OuterPlace[IO, Bounds, Error, T], f : Bounds => Bounds) : OuterPlace[IO, Bounds, Error, T] =
    StateTransformer.modifyScoped(original, f)
  end withBounds

  def raiseError[IO[_] : Monad, Bounds, Error, Value](error : => Error) : OuterPlace[IO, Bounds, Error, Value] =
    ErrorTransformer.raiseError[ReaderTransformer[Path] <> StateTransformer[Bounds], IO, Error, Value](error)
  end raiseError

  def run[IO[_] : Monad, Bounds, Error](path : Path, bounds : IO[Bounds]) : OuterPlaceC[IO, Bounds, Error] ~> EitherT[IO, Error, *] =
    new ~>[
      OuterPlaceC[IO, Bounds, Error], EitherT[IO, Error, *]
    ]:
      override def apply[A](fa: OuterPlaceC[IO, Bounds, Error][A]): EitherT[IO, Error, A] =
        EitherT.liftF(bounds).flatMap(fa.run(path).runA)
      end apply
    end new
  end run

  def currentPath[IO[_] : Monad, Bounds, Error] : OuterPlace[IO, Bounds, Error, Path] =
    ReaderTransformer.ask_
  end currentPath

  def addNameToPath[IO[_] : Monad, Bounds, Error](name : String)
    : OuterPlaceC[IO, Bounds, Error] ~> OuterPlaceC[IO, Bounds, Error] =
      new ~>[OuterPlaceC[IO, Bounds, Error], OuterPlaceC[IO, Bounds, Error]]:
        override def apply[A](fa: OuterPlaceC[IO, Bounds, Error][A]): OuterPlaceC[IO, Bounds, Error][A] =
          currentPath[IO, Bounds, Error].map(path => path / name).flatMap(
              path => ReaderTransformer.withValue_(fa, path)
          )
        end apply
      end new
  end addNameToPath
end OuterPlace

