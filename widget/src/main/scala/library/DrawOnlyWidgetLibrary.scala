package me.katze.gui4s.widget
package library

import library.lowlevel.WidgetLibrary

import cats.Monad
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.stateful.{BiMonad, Path}

def drawOnlyWidget(using lib : WidgetLibrary)(asFreeIn: lib.FreeWidget[Nothing, Any], drawIn: lib.Draw)(using BiMonad[lib.Update], Monad[lib.Merge]): lib.PlacedWidget[Nothing, Any] =
  lib.constructRealWidget[Nothing, Any](
    new PlacedWidget[lib.Update, lib.Merge, lib.Draw, lib.WidgetTask[Any], lib.FreeWidget, Nothing, Any]:
      override def handleDownEvent(event: Any): lib.Update[lib.FreeWidget[Nothing, Any], Nothing] = asFree.asMonad
  
      override def mergeWithState(oldState: Map[String, Any]): lib.Merge[lib.FreeWidget[Nothing, Any]] = asFree.pure[lib.Merge]
  
      override def childrenStates: Map[String, Any] = Map()
  
      override def filterDeadPaths(
                                    currentPath: Path,
                                    alive      : Set[Path]
                                  ): Set[Path] = alive

      override val asFree: lib.FreeWidget[Nothing, Any] = asFreeIn
      override val draw  : lib.Draw = drawIn
  )
end drawOnlyWidget
