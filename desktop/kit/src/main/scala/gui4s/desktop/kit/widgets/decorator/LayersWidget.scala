package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all._
import cats._
import cats.effect._

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.effects.{LinearContainerPlacementStrategy => _, _}
import gui4s.desktop.kit.widgets._

def layersWidget[IO[_] : Sync, Event](
                                       background : List[DesktopWidget[IO, Event]],
                                       foreground : List[DesktopWidget[IO, Event]],
                                       placementStrategy: PlacementStrategy[OuterPlaceC[IO], Rect[Float], List, Point2d[Float]]
                                    ) : Decorator[DesktopWidget[IO, Event]] =
 gui4s.desktop.widget.library.layersWidget[
   UpdateC[IO, Event],
   OuterPlaceC[IO],
   Draw[IO],
   RecompositionReaction[IO],
   DownEvent,
   Float
 ](
   containerWidget[IO, List, Event](traverseOrdered)(using Sync[IO], Traverse[List])
 )(background, foreground, placementStrategy)
end layersWidget

extension[IO[_], Event](value : DesktopWidget[IO, Event])
  def withBackground(
    background : DesktopWidget[IO, Event],
    placement: PlacementStrategy[OuterPlaceC[IO], Rect[Float], List, Point2d[Float]]
  )(using Sync[IO]) : DesktopWidget[IO, Event] =
     layersWidget[IO, Event](background.one, Nil, placement)(value)
  end withBackground

  def withForeground(
    foreground : DesktopWidget[IO, Event],
    placement: PlacementStrategy[OuterPlaceC[IO], Rect[Float], List, Point2d[Float]]
  )(using Sync[IO]) : DesktopWidget[IO, Event] =
     layersWidget[IO, Event](Nil, foreground.one, placement)(value)
  end withForeground
end extension
