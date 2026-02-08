package gui4s.core.layout.rowcolumn

import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Point3d, Rect}
import gui4s.core.layout.Weighted.{Rigid, Weight}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy, weightedRowColumnLayoutPlacement}
import gui4s.core.layout.{Placed, Sized, Weighted}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.should.Matchers.*

import scala.collection.immutable.List

final class WeightedRowColumnPlacementTest extends AnyFlatSpec:
  type Place[T] = TestPlace[T]

  def rowColumnPlace[Widget](
                              mainAxis : Axis,
                              children : List[Weighted[Place[Sized[Float, Widget]]]],
                              mainAxisStrategy : PlacementStrategy[Place, Float, Float, List, Float],
                              additionlAxisStrategy : OneElementPlacementStrategy[Place, Float, Float, Float]
                            ): Place[Sized[Float, List[Placed[Float, Widget]]]] =
    weightedRowColumnLayoutPlacement(
      State.get,
      State.set,
      _ - _,
      _ * _,
      mainAxis,
      children,
      PlacementStrategy.Zip(
        mainAxis,
        mainAxisStrategy,
        PlacementStrategy.PlaceListIndependently(additionlAxisStrategy)
      )
    )
  end rowColumnPlace

  def constSizeWidget(size : Rect[Float]) : Place[Sized[Float, Unit]] =
    Sized((), size).pure[Place]
  end constSizeWidget

  "weightedRowColumnLayoutPlacement" should "be zero size without any children" in:
    val children = List()
    runPlace(
      rowColumnPlace(
        Axis.Vertical,
        children,
        PlacementStrategy.Begin[Place, Float, List, Float](0f),
        OneElementPlacementStrategy.Begin[Place, Float, Float],
      ),
      Rect(0f, 0f)
    ) should be(Sized(List(), Rect(0f, 0f)))

  it should "place inside of itself correctly" in:
    val innerChildSize = Rect(5f, 5f)
    val horizontalChildrenCount = 5
    val verticalChildrenCount = 5
    val horizontalGap = 4f
    val verticalGap = 4f

    val innerChildren = List.fill(horizontalChildrenCount)(Rigid(constSizeWidget(innerChildSize)))
    val children =
      List.fill(verticalChildrenCount)(
        Rigid(
          rowColumnPlace(
            Axis.Horizontal,
            innerChildren,
            PlacementStrategy.SpaceBetween,
            OneElementPlacementStrategy.Begin[Place, Float, Float],
          )
        )
      )

    val bounds = Rect(
      horizontalChildrenCount * innerChildSize.width + (horizontalChildrenCount - 1) * horizontalGap,
      verticalChildrenCount * innerChildSize.height + (verticalChildrenCount - 1) * verticalGap
    )
    val childHeightWithGap = innerChildSize.height + verticalGap
    val childWidthWithGap = innerChildSize.width + horizontalGap

    def expectedInnerChildPlaced(point : Point3d[Float]) =
      Placed(
        (0 until horizontalChildrenCount).map(index =>
          Placed((), Point3d(childWidthWithGap * index, 0f, 0f), innerChildSize),
        ).toList,
        point,
        Rect(bounds.width, innerChildSize.height)
      )
    end expectedInnerChildPlaced

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

  given floatEquality: Equality[Float] =
    TolerantNumerics.tolerantFloatEquality(0.0001)

  given rectEquality: Equality[Rect[Float]] =
    (a: Rect[Float], b: Any) => b match
      case Rect(width : Float, height : Float) =>
        floatEquality.areEqual(a.width, width) && floatEquality.areEqual(a.height, height)
      case _ => false

  given pointEquality: Equality[Point3d[Float]] =
    (a, b) => b match
      case Point3d(x : Float, y : Float, z : Float) =>
          floatEquality.areEqual(a.x, x)
          && floatEquality.areEqual(a.y, y)
          && floatEquality.areEqual(a.z, z)
      case _ => false

  val sizeFillerChild: Place[Sized[Float, Unit]] = State.get.map(Sized((), _))

  it should "give children size according to their weights" in:
    val bounds = Rect(100f, 100f)
    val children = List(
      Weight(sizeFillerChild, 5),
      Weight(sizeFillerChild, 3),
      Weight(sizeFillerChild, 2),
    )

    val placed = runPlace(
      rowColumnPlace(
        Axis.Vertical,
        children,
        PlacementStrategy.Begin[Place, Float, List, Float](0f),
        OneElementPlacementStrategy.Begin[Place, Float, Float],
      ),
      bounds
    )

    placed.size should be(bounds)

    val ch1 :: ch2 :: ch3 :: Nil = placed.value.runtimeChecked

    assert(rectEquality.areEqual(ch1.size, Rect(100f, 50f)))
    assert(rectEquality.areEqual(ch2.size, Rect(100f, 30f)))
    assert(rectEquality.areEqual(ch3.size, Rect(100f, 20f)))

    assert(pointEquality.areEqual(ch1.coordinate, Point3d(0f, 0f, 0f)))
    assert(pointEquality.areEqual(ch2.coordinate, Point3d(0f, 50f, 0f)))
    assert(pointEquality.areEqual(ch3.coordinate, Point3d(0f, 80f, 0f)))

  it should "place rigid children first" in:
    val bounds = Rect(100f, 100f)
    val children = List(
      Rigid(constSizeWidget(Rect(100f, 10f))),
      Weight(sizeFillerChild, 1),
      Rigid(constSizeWidget(Rect(100f, 10f))),
    )
    val placed = runPlace(
      rowColumnPlace(
        Axis.Vertical,
        children,
        PlacementStrategy.Begin[Place, Float, List, Float](0f),
        OneElementPlacementStrategy.Begin[Place, Float, Float],
      ),
      bounds
    )

    placed.size should be(bounds)

    val ch1 :: ch2 :: ch3 :: Nil = placed.value.runtimeChecked

    assert(rectEquality.areEqual(ch1.size, Rect(100f, 10f)))
    assert(rectEquality.areEqual(ch2.size, Rect(100f, 80f)))
    assert(rectEquality.areEqual(ch3.size, Rect(100f, 10f)))

    assert(pointEquality.areEqual(ch1.coordinate, Point3d(0f, 0f, 0f)))
    assert(pointEquality.areEqual(ch2.coordinate, Point3d(0f, 10f, 0f)))
    assert(pointEquality.areEqual(ch3.coordinate, Point3d(0f, 90f, 0f)))
end WeightedRowColumnPlacementTest
