package gui4s.core.kit
package effects

import catnip.syntax.transformer.{*, given}
import catnip.transformer.*
import catnip.{Get, Set}
import cats.*
import cats.data.*
import catnip.transformer.ErrorTransformer.given
    

type OuterPlaceTansformer[Bounds, Error] =  StateTransformer[Bounds] <> ErrorTransformer[Error]
given[Bounds, Error]: MonadTransformer[OuterPlaceTansformer[Bounds, Error]] =
  composedMonadTransformerInstance[StateTransformer[Bounds], ErrorTransformer[Error]]

type OuterPlace[IO[_], Bounds, Error, Value] = OuterPlaceTansformer[Bounds, Error][IO, Value]

type OuterPlaceT[IO[_], Bounds, Error] = OuterPlace[IO, Bounds, Error, *]

trait OuterPlaceOps[IO[_] : Monad, Bounds, Error]:
  final given monadInstance : MonadError[OuterPlaceT[IO, Bounds, Error], Error] =
    summon

  final def liftK : IO ~> OuterPlaceT[IO, Bounds, Error] =
    MonadTransformer[OuterPlaceTansformer[Bounds, Error]].liftK
  end liftK

  final def liftF[Value](value : IO[Value]) : OuterPlace[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF

  final def getBounds: Get[OuterPlace[IO, Bounds, Error, *], Bounds] =
    StateTransformer.get_
  end getBounds

  final def setBounds: Set[OuterPlace[IO, Bounds, Error, *], Bounds] =
    StateTransformer.set_
  end setBounds
  
  final def withBounds[T](original : OuterPlace[IO, Bounds, Error, T], f : Bounds => Bounds) : OuterPlace[IO, Bounds, Error, T] =
    StateTransformer.modifyScoped_(original, f)
  end withBounds

  final def raiseError[Value](error : => Error) : OuterPlace[IO, Bounds, Error, Value] =
    ErrorTransformer.raiseError[StateTransformer[Bounds], IO, Error, Value](error)
  end raiseError

  final def run(bounds : IO[Bounds]) : OuterPlaceT[IO, Bounds, Error] ~> EitherT[IO, Error, *] =
    new ~>[
      OuterPlaceT[IO, Bounds, Error], EitherT[IO, Error, *]
    ]:
      override def apply[A](fa: OuterPlaceT[IO, Bounds, Error][A]): EitherT[IO, Error, A] =
        EitherT.liftF(bounds).flatMap(fa.eval)
      end apply
    end new
  end run
end OuterPlaceOps

