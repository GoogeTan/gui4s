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

object OuterPlace:
  given monadInstance[IO[_] : Monad, Bounds, Error] : MonadError[OuterPlaceT[IO, Bounds, Error], Error] =
    summon

  def liftK[IO[_] : Monad, Bounds, PlaceError] : IO ~> OuterPlaceT[IO, Bounds, PlaceError] =
    MonadTransformer[OuterPlaceTansformer[Bounds, PlaceError]].liftK
  end liftK

  def liftF[IO[_] : Monad, Bounds, Error, Value](value : IO[Value]) : OuterPlace[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad, Bounds, PlaceError]: Get[OuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    StateTransformer.get_
  end getBounds

  def setBounds[IO[_] : Monad, Bounds, PlaceError]: Set[OuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    StateTransformer.set_
  end setBounds
  
  def withBounds[IO[_] : Monad, Bounds, Error, T](original : OuterPlace[IO, Bounds, Error, T], f : Bounds => Bounds) : OuterPlace[IO, Bounds, Error, T] =
    StateTransformer.modifyScoped_(original, f)
  end withBounds

  def raiseError[IO[_] : Monad, Bounds, PlaceError, Value](error : => PlaceError) : OuterPlace[IO, Bounds, PlaceError, Value] =
    ErrorTransformer.raiseError[StateTransformer[Bounds], IO, PlaceError, Value](error)
  end raiseError

  def run[IO[_] : Monad, Bounds, PlaceError](bounds : IO[Bounds]) : OuterPlaceT[IO, Bounds, PlaceError] ~> EitherT[IO, PlaceError, *] =
    new ~>[
      OuterPlaceT[IO, Bounds, PlaceError], EitherT[IO, PlaceError, *]
    ]:
      override def apply[A](fa: OuterPlaceT[IO, Bounds, PlaceError][A]): EitherT[IO, PlaceError, A] =
        EitherT.liftF(bounds).flatMap(fa.eval)
      end apply
    end new
  end run
end OuterPlace

