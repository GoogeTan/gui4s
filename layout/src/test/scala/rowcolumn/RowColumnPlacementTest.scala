package me.katze.gui4s.layout
package rowcolumn

import bound.{Bounds, GetBounds, SetBounds}

import cats.*
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Axis, Point3d, Rect}
import rowcolumn.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.should.Matchers.*

import scala.annotation.experimental
import scala.collection.immutable.List

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

    it should "place inside of itself correctly" in:
        val innerChildSize = Rect(5f, 5f)
        val horizontalChildrenCount = 5
        val verticalChildrenCount = 5
        val horizontalGap = 4f
        val verticalGap = 4f

        val innerChildren : List[Place[Sized[MeasurementUnit, Unit]]] = List.fill(horizontalChildrenCount)(
            Sized((), innerChildSize).pure[Place]
        )
        val children : List[Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, Unit]]]]] = List.fill(verticalChildrenCount)(
            rowColumnPlace(
                Axis.Horizontal,
                innerChildren,
                MainAxisPlacement.SpaceBetween(PlacementError.BadStrategyForInfiniteSizeContainer),
                AdditionalAxisPlacement.Begin
            )
        )

        val bounds = Rect(
            horizontalChildrenCount * innerChildSize.width + (horizontalChildrenCount - 1) * horizontalGap,
            verticalChildrenCount * innerChildSize.height + (verticalChildrenCount - 1) * verticalGap
        )
        val childHeightWithGap = innerChildSize.height + verticalGap
        val childWidthWithGap = innerChildSize.width + horizontalGap

        def expectedInnerChildPlaced(point : Point3d[MeasurementUnit]) =
            Placed(
                (0 until horizontalChildrenCount).map(index =>
                    Placed((), Point3d(childWidthWithGap * index, 0f, 0f), innerChildSize),
                ).toList,
                point,
                Rect(bounds.width, innerChildSize.height)
            )
        runPlace(
            rowColumnPlace(
                Axis.Vertical,
                children,
                MainAxisPlacement.SpaceBetween(PlacementError.BadStrategyForInfiniteSizeContainer),
                AdditionalAxisPlacement.Begin
            ),
            new Bounds(bounds)
        ) should be(
            Right(
                Sized(
                    (0 until verticalChildrenCount).map(index =>
                        expectedInnerChildPlaced(Point3d(0f, childHeightWithGap * index, 0f)),
                    ).toList,
                    bounds
                )
            )
        )


end RowColumnPlacementTest
