package me.katze.gui4s.widget
package library

import library.lowlevel.WidgetLibrary
import me.katze.gui4s.widget.stateful.Path

def drawOnlyWidget(using lib : WidgetLibrary)(asFreeIn: lib.FreeWidget[Nothing, Any], drawIn: lib.Draw): lib.PlacedWidget[Nothing, Any] =
  lib.constructRealWidget[Nothing, Any](
    new PlacedWidget[lib.Draw, lib.WidgetTask[Any], lib.FreeWidget, Nothing, Any]:
      override def handleDownEvent(event: Any): EventResult[lib.WidgetTask[Any], lib.FreeWidget[Nothing, Any], Nothing] = EventResult(asFree)
  
      override def mergeWithState(oldState: Map[String, Any]): lib.FreeWidget[Nothing, Any] = asFree
  
      override def childrenStates: Map[String, Any] = Map()
  
      override def filterDeadPaths(
                                    currentPath: Path,
                                    alive      : Set[Path]
                                  ): Set[Path] = alive

      override val asFree: lib.FreeWidget[Nothing, Any] = asFreeIn
      override val draw  : lib.Draw = drawIn
)
