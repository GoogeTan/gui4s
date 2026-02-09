package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.*
import cats.*
import cats.effect.*
import gui4s.core.geometry.{InfinityOr, Point2d, Rect}
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.widgets.*

def layersWidget[IO[_] : Sync, Event](
                                       background : List[DesktopWidget[IO, Event]],
                                       foreground : List[DesktopWidget[IO, Event]],
                                       placementStrategy: PlacementStrategy[PlacementEffectC[IO], Rect[Float], Rect[Float], List, Point2d[Float]]
                                    ) : Decorator[DesktopWidget[IO, Event]] =
 gui4s.desktop.widget.library.layersWidget[
   UpdateC[IO, Event],
   PlacementEffectC[IO],
   Draw[IO],
   RecompositionReaction[IO],
   DownEvent,
   Float
 ](
   containerWidget[IO, List, Event](traverseOrdered)(using Sync[IO], Traverse[List]),
   bounds => PlacementEffect.withBoundsK(_ => bounds.map(new InfinityOr(_)))
 )(background, foreground, placementStrategy)
end layersWidget

extension[IO[_], Event](value : DesktopWidget[IO, Event])
  def withBackground(using Sync[IO])(
    background : DesktopWidget[IO, Event],
    placement: PlacementStrategy[PlacementEffectC[IO], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : DesktopWidget[IO, Event] =
     layersWidget[IO, Event](
       background.one,
       Nil,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withBackground

  def withForeground(using Sync[IO])(
    foreground : DesktopWidget[IO, Event],
    placement: PlacementStrategy[PlacementEffectC[IO], Rect[Float], Rect[Float], Id, Point2d[Float]]
  ) : DesktopWidget[IO, Event] =
     layersWidget[IO, Event](
       Nil,
       foreground.one,
       PlacementStrategy.PlaceStackIndependently(placement)
     )(value)
  end withForeground
end extension
