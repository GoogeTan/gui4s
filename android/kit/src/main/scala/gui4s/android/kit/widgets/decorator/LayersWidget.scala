package gui4s.android.kit.widgets.decorator

import catnip.syntax.all.{_, given}
import cats.*
import cats.effect.*

import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.widget.library.decorator.Decorator

import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.PlacementEffect.given
import gui4s.android.kit.effects.RecompositionReaction.given
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*

def layersWidget[Event](
                         background : List[AndroidWidget[Event]],
                         foreground : List[AndroidWidget[Event]],
                         placementStrategy: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], List, Point2d[Float]]
                       ) : Decorator[AndroidWidget[Event]] =
 gui4s.desktop.widget.library.layersWidget[
   UpdateC[Event],
   PlacementEffect,
   Draw,
   RecompositionReaction,
   Float,
   InfinityOr[Float]
 ](
   listContainerWidget[Event],
   bounds => new ~>[PlacementEffect, PlacementEffect]:
     def apply[A](fa : PlacementEffect[A]) : PlacementEffect[A] =
       PlacementEffect.withBounds[A](fa, _ => bounds.map(new InfinityOr(_)))
 )(background, foreground, placementStrategy)
end layersWidget

extension[Event](value : AndroidWidget[Event])
  def withBackground(
    background : AndroidWidget[Event],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : AndroidWidget[Event] =
     layersWidget[Event](
       background.one,
       Nil,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withBackground

  def withForeground(
    foreground : AndroidWidget[Event],
    placement: PlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : AndroidWidget[Event] =
     layersWidget[Event](
       Nil,
       foreground.one,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withForeground

  def withBackground(
    background: AndroidWidget[Event],
  ): AndroidWidget[Event] =
    withBackground(background, OneElementPlacementStrategy.Begin, OneElementPlacementStrategy.Begin)
  end withBackground

  def withBackground(
    background: AndroidWidget[Event],
    horizontalPlacementStrategy : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
    verticalPlacementStrategy   : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
  ): AndroidWidget[Event] =
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
    foreground: AndroidWidget[Event],
  ): AndroidWidget[Event] =
    withForeground(foreground, OneElementPlacementStrategy.Begin, OneElementPlacementStrategy.Begin)
  end withForeground

  def withForeground(
    foreground: AndroidWidget[Event],
    horizontalPlacementStrategy : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
    verticalPlacementStrategy   : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
  ): AndroidWidget[Event] =
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
