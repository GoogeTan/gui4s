package me.katze.gui4s.example

import api.impl.{DrawMonad, HighLevelApiImpl, LayoutApiImpl, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.{SimpleDrawApi, SwingApi, SwingProcessRequest, swingBounds}
import place.{additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}
import task.{IOOnThread, MultiMap, RefTaskSet, StlWrapperMultiMap}
import update.ApplicationRequest

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.impl
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}
import update.ProcessRequest

import cats.effect.std.Queue

import scala.math.Numeric.Implicits.*

type Draw[MU, T] = ReaderT[IO, (MU, MU), T]
type DrawT[MU] = [T] =>> Draw[MU, T]

given[MU](using drawMonad: DrawMonad[DrawT[MU], MU]): LayoutDraw[Draw[MU, Unit], LayoutPlacementMeta[MU]] with
  override def drawChildren(children: List[(Draw[MU, Unit], LayoutPlacementMeta[MU])]): Draw[MU, Unit] =
    children.traverse_((childDraw, coords) => drawMonad.move(coords.x, coords.y, childDraw))
  end drawChildren
end given

given [MU: Numeric] : DrawMonad[DrawT[MU], MU] with
  override def move[T](dx: MU, dy: MU, effect: Draw[MU, T]): Draw[MU, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given

type HL[W[+_], WT[+_], MU] <: HighLevelApi & LabelApi[Unit] & LayoutApi[MU]
  {
    type Widget[+T] = W[T]
    type WidgetTask[+T] = WT[T]
  }

type DownEvent = TaskFinished

trait Gui4sApp[MU : Fractional] extends IOApp:
  final override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- SwingApi.invoke
      lowLevelLib = WidgetLibraryImpl[IO, Draw[MU, Unit], MeasurableT[MU], WidgetTaskT[IO], DownEvent]()
      taskSet <- Ref.of[IO, MultiMap[Path, IOOnThread[IO]]](StlWrapperMultiMap(Map()))
      given ProcessRequest[IO, ApplicationRequest] = SwingProcessRequest(swing)
      code <- runWidget(lowLevelLib)(
        widget = queue =>
          createRootWidget(lowLevelLib)(
            freeWidget = app(using higherApi(lowLevelLib, swing.graphics)),
            taskSet = RefTaskSet(
              runningTaskSet = taskSet,
              startTask = (path, task) => startWidgetTask(
                task = task,
                resultDrain = offerTask(queue, at = path, _)
              )
            )
          )(using summon, MeasurableRunPlacement(swingBounds(swing))),
        drawLoopExceptionHandler = drawLoopExceptionHandler,
        api = swing.graphics,
        runDraw = _.run(Numeric[MU].zero, Numeric[MU].zero)
      )
    yield code
  end run

  def offerTask[F[+_]](queue: Queue[F, TaskFinished], at: Path, taskResult: Any) : F[Unit] =
    queue.offer(TaskFinished(at, taskResult))
  end offerTask

  def startWidgetTask[F[+_] : Concurrent, T](task: WidgetTaskImpl[F, T], resultDrain: T => F[Unit]) : F[Fiber[F, Throwable, Unit]] =
    // TODO add execution context
    Concurrent[F].start(runWidgetTask(task, resultDrain))
  end startWidgetTask

  def runWidgetTask[F[+_] : Concurrent, T](widgetTask : WidgetTaskImpl[F, T], offerTask : T => F[Unit]) : F[Unit] =
    widgetTask match
      case WidgetTaskImpl.OneEvent(value) => value.flatMap(offerTask)
      case WidgetTaskImpl.ManyEvents(stream) => stream.evalMap(offerTask).compile.drain
    end match
  end runWidgetTask


  type TextStyle = Unit

  private def higherApi(lowLevelApi: WidgetLibraryImpl[IO, Draw[MU, Unit], MeasurableT[MU], WidgetTaskT[IO], DownEvent], drawApi: SimpleDrawApi[MU, Draw[MU, Unit]]) : HL[lowLevelApi.Widget, lowLevelApi.WidgetTask, MU] =
    given LabelPlacement[Measurable[MU, LayoutPlacementMeta[MU]], TextStyle] with
      override def sizeText(text : String, options: TextStyle): Measurable[MU, LayoutPlacementMeta[MU]] =
        _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(10))
    given lowLevelApi.type = lowLevelApi

    new HighLevelApiImpl[IO, DrawT[MU], MeasurableT[MU], WidgetTaskT[IO], MU, TextStyle, DownEvent](using lowLevelApi)(drawApi) with LayoutApiImpl[MU](
      [Event] => (axis, elements, main, additional) => weightedRowColumnPlace(
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, mainAxisStrategyPlacement(main, _, _), additionalAxisStrategyPlacement(additional, _, _))
      ).map(unpack)
    ) {
      override val wl: WidgetLibraryImpl[IO, DrawT[MU][TextStyle], MeasurableT[MU], WidgetTaskT[IO], DownEvent] = lowLevelApi
    }.asInstanceOf[HL[lowLevelApi.Widget, lowLevelApi.WidgetTask, MU]] // TODO do not believe me
  end higherApi

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler

  def app(using api : HighLevelApi & LayoutApi[MU] & LabelApi[Unit]) : api.Widget[ApplicationRequest]
end Gui4sApp
