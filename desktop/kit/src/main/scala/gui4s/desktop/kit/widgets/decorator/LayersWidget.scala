package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.{_, given}
import cats._
import cats.effect._

import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._

def layersWidget[Event](
                         background : List[DesktopWidget[Event]],
                         foreground : List[DesktopWidget[Event]],
                         placementStrategy: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], List, Point2d[Float]]
                       ) : Decorator[DesktopWidget[Event]] =
 gui4s.desktop.widget.library.layersWidget[
   UpdateC[Event],
   PlacementEffect,
   Draw,
   RecompositionReaction,
   DownEvent,
   Float,
   InfinityOr[Float]
 ](
   listContainerWidget[Event],
   bounds => PlacementEffect.withBoundsK(_ => bounds.map(new InfinityOr(_)))
 )(background, foreground, placementStrategy)
end layersWidget

extension[Event](value : DesktopWidget[Event])
  def withBackground(
    background : DesktopWidget[Event],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : DesktopWidget[Event] =
     layersWidget[Event](
       background.one,
       Nil,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withBackground

  def withForeground(
    foreground : DesktopWidget[Event],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : DesktopWidget[Event] =
     layersWidget[Event](
       Nil,
       foreground.one,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withForeground

  def withBackground(
    background: DesktopWidget[Event],
    horizontalPlacementStrategy : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float] = OneElementPlacementStrategy.Begin,
    verticalPlacementStrategy   : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float] = OneElementPlacementStrategy.Begin,
  ): DesktopWidget[Event] =
    layersWidget[Event](
      background.one,
      Nil,
      PlacementStrategy.PlaceStackIndependently(
        PlacementStrategy.Zip[
          PlacementEffect,
          Float,
          Float,
          Id,
          Float
        ](
          horizontalPlacementStrategy,
          verticalPlacementStrategy
        )
      )
    )(value)
  end withBackground

  def withForeground(
    foreground: DesktopWidget[Event],
    horizontalPlacementStrategy : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float] = OneElementPlacementStrategy.Begin,
    verticalPlacementStrategy   : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float] = OneElementPlacementStrategy.Begin,
  ): DesktopWidget[Event] =
    layersWidget[Event](
      Nil,
      foreground.one,
      PlacementStrategy.PlaceStackIndependently(
        PlacementStrategy.Zip[
          PlacementEffect,
          Float,
          Float,
          Id,
          Float
        ](
          horizontalPlacementStrategy,
          verticalPlacementStrategy
        )
      )
    )(value)
  end withForeground
end extension
