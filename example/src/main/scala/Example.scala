package me.katze.gui4s.example

import cats.effect.IO
import me.katze.gui4s.example
import me.katze.gui4s.widget.impl.{FreeStatefulFabricImpl, WidgetLibraryImpl}
import me.katze.gui4s.widget.library.{LabelDraw, LabelLibrary, LabelPlacement, StatefulLibrary}
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.{EventReaction, FreeStatefulFabric, RichTypeChecker, State, StatefulDraw, TaskFinished}
import me.katze.gui4s.widget.{*, given}


object lib extends WidgetLibraryImpl[IO, String] with StatefulLibrary with LabelLibrary[Unit]:
  override def statefulFabric[RaiseableEvent, HandleableEvent >: TaskFinished, ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent](using RichTypeChecker[ChildRaiseableEvent]) =
      FreeStatefulFabricImpl[String, WidgetTask, Magic, PlacementEffect, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent](constructRealWidget[RaiseableEvent, HandleableEvent])(using statefulIsDrawable, placementIsEffect, freeTreesAreMergeable)

  override given statefulIsDrawable: StatefulDraw[String] = new StatefulDraw[String]:
    override def drawStateful[T](
                                  name     : String,
                                  state    : State[Any, T, Any, Any],
                                  childTree: me.katze.gui4s.widget.PlacedWidget[String, Any, [A, B] =>> Any, Any, Nothing]
                                ): String =
      s"Stateful(name=$name; state=$state)\n  ${childTree.draw}"
  end statefulIsDrawable

  override def textIsPlaceable: LabelPlacement[Placeable[Unit]] = text => bounds => ()

  override def textDraw: LabelDraw[String, Unit] = (text, _) => s"Label($text)"
end lib

object Example extends App:
  import lib.*

  final case class Taps(count : Int)

  enum TapsEvent:
    case Inc, Dec
  end TapsEvent

  private val handleTapEvent = (state: Taps, event: TapsEvent) =>
    event match
      case TapsEvent.Inc => EventReaction(Taps(state.count + 1))
      case TapsEvent.Dec => EventReaction(Taps(state.count - 1))
    end match
  end handleTapEvent

  val tree: FreeWidget[Nothing, TaskFinished] = lib.stateful(
    "outer",
    Taps(0),
    handleTapEvent,
    outerState => stateful("inner", Taps(0), handleTapEvent, (state: Taps) => label(s"$outerState $state"))
  )


  def runEvent(widget: PlacedWidget[Nothing, TaskFinished], event: TaskFinished): PlacedWidget[Nothing, TaskFinished] =
    widget.handleDownEvent(event).widget.place(null)

  private def runEvents(widget: PlacedWidget[Nothing, TaskFinished], events: List[TaskFinished]): PlacedWidget[Nothing, TaskFinished] =
    events.foldLeft(widget)(runEvent)

  val placedTree = tree.place(null)

  println(
    runEvents(
        placedTree,
        List(
          TaskFinished("outer", List("inner", "end"), TapsEvent.Inc),
          TaskFinished("outer", List("inner", "end"), TapsEvent.Inc),
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
          TaskFinished("outer", Nil, TapsEvent.Inc)
        )
    ).draw
  )
end Example
