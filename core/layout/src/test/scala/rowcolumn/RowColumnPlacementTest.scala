package gui4s.core.layout
package rowcolumn

import cats.*
import cats.data.*
import cats.syntax.all.*
import catnip.syntax.all.given 
import gui4s.core.geometry.{Axis, Point3d, Rect}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.should.Matchers.*

import scala.collection.immutable.List

final class RowColumnPlacementTest extends AnyFlatSpec:
    type MeasurementUnit = Float
    type Place[T] = State[Rect[MeasurementUnit], T]

    def runPlace[T](value : Place[T], bounds : Rect[MeasurementUnit]) : T =
        value.runA(bounds).value
    end runPlace

    def rowColumnPlace[Widget](
                                mainAxis : Axis,
                                children : List[Place[Sized[MeasurementUnit, Widget]]],
                                mainAxisStrategy : PlacementStrategy[Place, Float, List, MeasurementUnit],
                                additionlAxisStrategy : OneElementPlacementStrategy[Place, Float, MeasurementUnit]
    ): Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, Widget]]]] =
        rowColumnLayoutPlacement[Place, List, Widget, Float, MeasurementUnit](
            State.get,
            State.set,
            _ - _,
            mainAxis,
            children,
            PlacementStrategy.Zip(
                mainAxis,
                mainAxisStrategy,
                PlacementStrategy.OneByOne(
                    additionlAxisStrategy,
                )
            )
        )

    "rowColumnPlace" should "be zero size without any children" in:
        val children = List()
        runPlace(
            rowColumnPlace(
                Axis.Vertical,
                children,
                PlacementStrategy.Begin[Place, Float, List, Float](0f),
                OneElementPlacementStrategy.Begin[Place, Float, Float],
            ),
            Rect(0f, 0f) 
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
                PlacementStrategy.SpaceBetween,
                OneElementPlacementStrategy.Begin[Place, Float, Float],
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
                PlacementStrategy.SpaceBetween,
                OneElementPlacementStrategy.Begin[Place, Float, Float],
            ),
            bounds
        ) should be(
            Sized(
                (0 until verticalChildrenCount).map(index =>
                    expectedInnerChildPlaced(Point3d(0f, childHeightWithGap * index, 0f)),
                ).toList,
                bounds
            )
        )
end RowColumnPlacementTest
