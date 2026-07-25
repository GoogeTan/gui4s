package gui4s.desktop.widget.library

import cats.*
import cats.syntax.all.*
import scala.reflect.Typeable
import gui4s.core.widget.StatefulState

trait WidgetTestingSpec:
  type Update[Event, T]
  given updateMonad[Event]: Monad[Update[Event, *]]
  type Place[T]
  type RecompositionReaction
  type Draw
  type IO[T]
  given ioMonad : Monad[IO]

  type PlacedTestWidget[Event] = Widget[Update[Event, *], Place, Draw, RecompositionReaction]
  type TestWidget[Event] = Place[Widget[Update[Event, *], Place, Draw, RecompositionReaction]]

  //TODO extract this into widget library module to reuse with same code in desktop kit
  def typeCheckState[State : Typeable](error : [T] => Any => Place[T]) =
    [T] => (a: Any, f: StatefulState[State] => Option[Place[T]]) =>
      a match
        case StatefulState(initial : State, current: State) =>
          f(StatefulState(initial, current))
        case _ => Some(error(a))
  end typeCheckState

  def runPlace : Place ~> IO
  def runUpdate[T, Event](value : Update[Event, T]) : IO[(List[Event], T)]
  def runRecomposition : RecompositionReaction => IO[Unit]

  def processEvent[Event](testWidget : PlacedTestWidget[Event]) : Update[Event, IO[PlacedTestWidget[Event]]] =
    gui4s.desktop.widget.library.processEvent(runPlace, runRecomposition, testWidget)
  end processEvent

  def processEventAndRunUpdate[Event](testWidget : PlacedTestWidget[Event]) : IO[(List[Event], PlacedTestWidget[Event])] =
    runUpdate(processEvent(testWidget))
      .flatMap((events, ioWidget) => ioWidget.map((events, _)))
  end processEventAndRunUpdate

  def processEventAndRunUpdateForFreeWidget[Event](testWidget : TestWidget[Event]) : IO[(List[Event], PlacedTestWidget[Event])] =
    runPlace(testWidget).flatMap(processEventAndRunUpdate)
  end processEventAndRunUpdateForFreeWidget
end WidgetTestingSpec
