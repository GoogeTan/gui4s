package me.katze.gui4s.example

import root.{RootWidgetFree, RootWidgetPlaced}

import cats.Monad
import me.katze.gui4s.widget.impl.WidgetLibraryImpl

trait RootLibrary[F[+_] : Monad, Draw, Bounds](using place.ApplicationBounds[F, Bounds]) extends WidgetLibraryImpl[F, Draw, Bounds]:
  type FreeRootWidget[UpEvent, DownEvent] = RootWidgetFree[F, Draw, WidgetTask[Any], Bounds, FreeWidget, UpEvent, DownEvent]
  type PlacedRootWidget[UpEvent, DownEvent] = RootWidgetPlaced[F, Draw, Bounds, FreeWidget, FreeRootWidget, WidgetTask[Any], UpEvent, DownEvent]

  def createRootWidget[UpEvent, DownEvent](
                    measurable: PlacementEffect[PlacedWidget[UpEvent, DownEvent]],
                    master : IOMaster[F, WidgetTask[Any]],
                  ) : FreeRootWidget[UpEvent, DownEvent] =
    RootWidgetFree(
      measurable,
      master,
      RootWidgetPlaced(_, _, createRootWidget)
    )
  end createRootWidget
end RootLibrary
