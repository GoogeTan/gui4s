package me.katze.gui4s.widget
package effect
import stateful.Path

final class LaunchedEffectWidget[
  +Draw,
  +WidgetTask,
  +FreeWidget[+_, -_],
  +RaiseableEvent
](
  keys : Array[Any],
  noDraw : Draw
) extends PlacedWidget[Draw, WidgetTask, FreeWidget, RaiseableEvent, Any]:
  override def filterDeadPaths(currentPath: Path, alive : Set[Path]): Set[Path] = alive

  override def handleDownEvent(event: Any): EventResult[WidgetTask, FreeWidget[RaiseableEvent, Any], RaiseableEvent] =
    EventResult(asFree)
  end handleDownEvent

  override def mergeWithState(oldState: Map[String, Any]): FreeWidget[RaiseableEvent, Any] =
    ???
  end mergeWithState

  override def childrenStates: Map[String, Any] =
    Map()
  end childrenStates

  override def asFree: FreeWidget[RaiseableEvent, Any] =
    ???
  end asFree

  override def draw: Draw = noDraw
end LaunchedEffectWidget

