package me.katze.gui4s.example

import cats.data.State as StateMonad
import cats.effect.IO
import me.katze.gui4s.example
import me.katze.gui4s.widget.impl.{FreeStatefulFabricImpl, WidgetLibraryImpl}
import me.katze.gui4s.widget.library.{LabelDraw, LabelLibrary, LabelPlacement, StatefulLibrary}
import me.katze.gui4s.widget.placeable.Placeable
import me.katze.gui4s.widget.stateful.*
import me.katze.gui4s.widget.{*, given}

object lib extends WidgetLibraryImpl[IO, StateMonad[Int, String], Null] with StatefulLibrary with LabelLibrary[Unit]:
  override def statefulFabric[
    RaiseableEvent, HandleableEvent >: TaskFinished,
    ChildRaiseableEvent, ChildHandleableEvent >: HandleableEvent
  ](using RichTypeChecker[ChildRaiseableEvent]): FreeStatefulFabricImpl[StateMonad[Int, String], WidgetTask, Magic, [T] =>> Placeable[Null, T], RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent] =
      FreeStatefulFabricImpl[StateMonad[Int, String], WidgetTask, Magic, PlacementEffect, RaiseableEvent, HandleableEvent, ChildRaiseableEvent, ChildHandleableEvent](constructRealWidget[RaiseableEvent, HandleableEvent])(using statefulIsDrawable, placementIsEffect, freeTreesAreMergeable)

  override given statefulIsDrawable: StatefulDraw[StateMonad[Int, String]] = new StatefulDraw[StateMonad[Int, String]]:
    override def drawStateful[T](
                                  name     : String,
                                  state    : State[Any, T, Any, Any],
                                  childTree: me.katze.gui4s.widget.PlacedWidget[StateMonad[Int, String], Any, [A, B] =>> Any, Any, Nothing]
                                ): StateMonad[Int, String] =
      for
        tabs <- StateMonad.get[Int]
        res = "  ".repeat(tabs) + s"Stateful(name=$name; state=$state)\n"
        _ <- StateMonad.modify[Int](_ + 1)
        child <- childTree.draw
        _ <- StateMonad.modify[Int](_ - 1)
        tail = "  ".repeat(tabs) + s"end $name\n"
      yield res + child + tail
  end statefulIsDrawable

  override def textIsPlaceable: LabelPlacement[Placeable[Null, Unit]] = text => bounds => ()

  override def textDraw: LabelDraw[StateMonad[Int, String], Unit] =
    (text, _) =>
      for
        tabs <- StateMonad.get
      yield "  ".repeat(tabs) + s"Label($text)\n"
  end textDraw
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

  val tree: Widget[Nothing] =
    stateful("outer", Taps(0), handleTapEvent): outerState =>
      stateful("inner", Taps(0), handleTapEvent): innerState =>
        label(s"$outerState $innerState")


  def runEvent(widget: PlacedWidget[Nothing, TaskFinished], event: TaskFinished): PlacedWidget[Nothing, TaskFinished] =
    widget.handleDownEvent(event).widget.place(null)

  private def runEvents(widget: PlacedWidget[Nothing, TaskFinished], events: List[TaskFinished]): PlacedWidget[Nothing, TaskFinished] =
    events.foldLeft(widget)(runEvent)

  val placedTree = tree.place(null)

  println(
    runEvents(
        placedTree,
        List(
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
          TaskFinished("outer", List("inner"), TapsEvent.Inc),
        )
    ).draw.runA(0).value
  )
end Example
