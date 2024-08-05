package me.katze.gui4s.example

import cats.effect.IO
import me.katze.gui4s.example.impl.StatefulFabricImpl.{FreeT, Magic}
import me.katze.gui4s.example.impl.{StatefulFabricImpl, WidgetTaskImpl}
import me.katze.gui4s.example.placeable.{Bounds, Placeable}
import me.katze.gui4s.example.stateful.{EventReaction, RichTypeChecker, TaskFinished}

import scala.reflect.Typeable

object Example extends App:
  given [T: Typeable]: RichTypeChecker[T] = value => summon[Typeable[T]].unapply(value).toRight("Cast failed")

  final case class Taps(count : Int)

  given[T : Typeable]: Typeable[(T, T)] = a => a match
    case (b, c) =>
      for
        bb <- summon[Typeable[T]].unapply(b)
        cc <- summon[Typeable[T]].unapply(c)
      yield (bb, cc).asInstanceOf[(T, T) & a.type]
    case _ => None
  end given

  enum TapsEvent:
    case Inc, Dec

  class Label(text : String) extends PlacedWidget[WidgetTaskImpl[IO, Any], FreeT[Magic], TapsEvent, TaskFinished]:
    override def handleDownEvent(event: TaskFinished): EventResult[WidgetTaskImpl[IO, Any], Placeable[Magic[TapsEvent, TaskFinished]], TapsEvent] =
      event match
        case TaskFinished("end", Nil, value : TapsEvent) => EventResult(asFree, Some(value))
        case _ => EventResult(asFree)
      end match
    end handleDownEvent

    override def mergeWithState(oldState: Map[String, Any]) = asFree
    override def asFree = bounds => Magic(this)
    override def childrenStates: Map[String, Any] = Map()

    override def prettyString: String = s"Text(${text})"
  end Label

  val handleTapEvent = (state : Taps, event : TapsEvent) =>
    event match
      case TapsEvent.Inc => EventReaction(Taps(state.count + 1))
      case TapsEvent.Dec => EventReaction(Taps(state.count - 1))


  def runEvent(widget: Magic[Nothing, TaskFinished], event: TaskFinished): Magic[Nothing, TaskFinished] =
    widget.handleDownEvent(event).widget.place(null)

  private def runEvents(widget: Magic[Nothing, TaskFinished], events: List[TaskFinished]): Magic[Nothing, TaskFinished] =
    events.foldLeft(widget)(runEvent)

  val tree: Placeable[Magic[Nothing, TaskFinished]] = StatefulFabricImpl.stateful(
    "outer",
    Taps(0),
    handleTapEvent,
    outerState => (bounds : Bounds) => Magic(StatefulFabricImpl.stateful("inner", Taps(0), handleTapEvent, (state : Taps) => _ => Magic(Label(s"${outerState} ${state}"))).place(bounds))
  )

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
    ).prettyString
  )
