package gui4s.desktop.widget.library

import catnip.syntax.all.nestedFunctorsAreFunctors
import cats.data.{Reader, ReaderT}
import cats.Functor
import gui4s.core.geometry.{Point3d, Rect}
import gui4s.core.layout.Sized.given
import gui4s.core.layout.{Measured, PlacementStrategy, Sized}
import gui4s.desktop.widget.library.Widget
import org.scalatest.flatspec.AnyFlatSpec

class ContainerTest extends AnyFlatSpec:
  type Update[T] = T
  type PlacementEffect[T] = Reader[Rect[Float], T]
  type Place[T] = PlacementEffect[Sized[Rect[Float], T]]
  given Functor[Place] = nestedFunctorsAreFunctors[PlacementEffect, Sized[Rect[Float], *]]
  type Draw = List[Any]// Запоминаем порядок вызова
  type RecompositionReaction = Unit
  type EnvironmentalEvent = Unit

  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
  type FreeWidget = Place[PlacedWidget]

  val getBounds : PlacementEffect[Rect[Float]] = ReaderT.ask
  
  def leafWidget(name : Sized[Rect[Float], String]) : FreeWidget =
    gui4s.desktop.widget.library.leafWidget(
      Reader(_ => name),
      List(name),
      ()
    )
  end leafWidget

  def listContainer(
    children : List[FreeWidget],
    placementStrategy :  PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[Rect[Float], Rect[Float], PlacedWidget]],
      Rect[Float],
      Rect[Float],
      List,
      (PlacedWidget, Measured[Rect[Float], Rect[Float], Point3d[Float]])
    ]
  ) : FreeWidget =
    ???
  end listContainer
  
  "Empty container" should "always be incremental" in {
    
  }
end ContainerTest