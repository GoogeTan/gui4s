package me.katze.gui4s
package example.api.effects

import catnip.{Get, Set}
import catnip.transformer.*
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*

type SkijaOuterPlaceTransformer[Bounds, Error] =  StateTransformer[Bounds] <> ErrorTransformer[Error]

type SkijaOuterPlace[IO[_], BoundUnit, Error, Value] = SkijaOuterPlaceTransformer[BoundUnit, Error][IO, Value]

type SkijaOuterPlaceT[IO[_], BoundUnit, Error] = SkijaOuterPlace[IO, BoundUnit, Error, *]

object SkijaOuterPlace:
  given monadInstance[IO[_] : Monad, Bounds, Error] : Monad[SkijaOuterPlaceT[IO, Bounds, Error]] =
    monadInstanceForTransformer

  def liftK[IO[_] : Monad, Bounds, PlaceError] : IO ~> SkijaOuterPlaceT[IO, Bounds, PlaceError] =
    MonadTransformer[SkijaOuterPlaceTransformer[Bounds, PlaceError]].liftK
  end liftK

  def liftF[IO[_] : Monad, Bounds, Error, Value](value : IO[Value]) : SkijaOuterPlace[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF

  def getBounds[IO[_] : Monad, Bounds, PlaceError]: Get[SkijaOuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    StateTransformer.get_
  end getBounds

  def setBounds[IO[_] : Monad, Bounds, PlaceError]: Set[SkijaOuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    StateTransformer.set_
  end setBounds
  
  def withBounds[IO[_] : Monad, Bounds, Error, T](original : SkijaOuterPlace[IO, Bounds, Error, T], f : Bounds => Bounds) : SkijaOuterPlace[IO, Bounds, Error, T] =
    StateTransformer.modifyScoped_(original, f)
  end withBounds

  def raiseError[IO[_] : Monad, Bounds, PlaceError, Value](error : => PlaceError) : SkijaOuterPlace[IO, Bounds, PlaceError, Value] =
    ErrorTransformer.raiseError[StateTransformer[Bounds], IO, PlaceError, Value](error)
  end raiseError

  def run[IO[_] : Monad, Bounds, PlaceError](bounds : IO[Bounds]) : SkijaOuterPlaceT[IO, Bounds, PlaceError] ~> EitherT[IO, PlaceError, *] =
    new ~>[
      SkijaOuterPlaceT[IO, Bounds, PlaceError], EitherT[IO, PlaceError, *]
    ]:
      override def apply[A](fa: SkijaOuterPlaceT[IO, Bounds, PlaceError][A]): EitherT[IO, PlaceError, A] =
        EitherT.liftF(bounds).flatMap(fa.eval)
      end apply
    end new
  end run
end SkijaOuterPlace

