package me.katze.gui4s.layout.rowcolumn

import org.scalatest.flatspec.*
import org.scalatest.*

import scala.collection.immutable.List
import cats.*
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Axis, Rect}
import me.katze.gui4s.layout.bound.{Bounds, GetBounds, SetBounds}
import me.katze.gui4s.layout.*
import me.katze.gui4s.layout.rowcolumn.*
import org.scalatest.matchers.should.Matchers.*

import scala.annotation.experimental

@experimental
final class RowColumnPlacementTest extends AnyFlatSpec:
    type MeasurementUnit = Float
    enum PlacementError:
        case BadStrategyForInfiniteSizeContainer
    type Place[T] = EitherT[State[Bounds[MeasurementUnit], *], PlacementError, T]

    object Place:
        val getBounds : GetBounds[Place, MeasurementUnit] = GetBounds.liftK[State[Bounds[MeasurementUnit], *], Place, MeasurementUnit](
            GetBounds.getBoundsStateT[Eval, MeasurementUnit],
            EitherT.liftK[State[Bounds[MeasurementUnit], *], PlacementError]
        )
        val setBounds : SetBounds[Place, MeasurementUnit] = SetBounds.liftK[State[Bounds[MeasurementUnit], *], Place, MeasurementUnit](
            SetBounds.setBoundsStateT[Eval, MeasurementUnit],
            EitherT.liftK[State[Bounds[MeasurementUnit], *], PlacementError]
        )
    end Place

    def runPlace[T](value : Place[T], bounds : Bounds[MeasurementUnit]) : Either[PlacementError, T] =
        value.value.run(bounds).value._2
    end runPlace

    def rowColumnPlace[Widget](
        mainAxis : Axis,
        children : List[Place[Sized[MeasurementUnit, Widget]]],
        mainAxisStrategy : MainAxisPlacement[Place, MeasurementUnit],
        additionlAxisStrategy : AdditionalAxisPlacement[Place, MeasurementUnit]
    ): Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, Widget]]]] =
        rowColumnLayoutPlacement[Place, Widget, MeasurementUnit](
            Place.getBounds,
            Place.setBounds,
            mainAxis,
            children,
            mainAxisStrategy,
            additionlAxisStrategy
        )

    "rowColumnPlace" should "be zero size without any children" in:
        val children = List()
        runPlace(
            rowColumnPlace(
                Axis.Vertical,
                children,
                MainAxisPlacement.Begin(0f),
                AdditionalAxisPlacement.Begin
            ),
            new Bounds(None, None)
        ) should be(Right(Sized(List(), Rect(0f, 0f))))
end RowColumnPlacementTest
