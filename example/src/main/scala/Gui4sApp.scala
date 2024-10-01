package me.katze.gui4s.example

import api.impl.{DrawMonad, HighLevelApiImpl, LayoutApiImpl, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.{SimpleDrawApi, SwingApi, SwingProcessRequest, SwingRunPlacement}
import update.ApplicationRequest

import cats.data.ReaderT
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.{*, given}
import place.{additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}

import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl

import scala.math.Fractional.Implicits.*
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

trait Gui4sApp[MU : Fractional] extends IOApp:
  final override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- SwingApi.invoke
      lowLevelLib = WidgetLibraryImpl[IO, Draw[MU, Unit], MeasurableT[MU]]()
      code <- runWidget(lowLevelLib)(
        widget = app(using hegherApi(lowLevelLib, swing.graphics)),
        drawLoopExceptionHandler = drawLoopExceptionHandler,
        api = swing.graphics,
        runDraw = _.run(Numeric[MU].zero, Numeric[MU].zero)
      )(using summon, SwingProcessRequest(swing), SwingRunPlacement(swing))
    yield code
  end run

  type TextStyle = Unit

  private def hegherApi(lowLevelApi: WidgetLibraryImpl[IO, Draw[MU, Unit], MeasurableT[MU]], drawApi: SimpleDrawApi[MU, Draw[MU, Unit]]) : HL[lowLevelApi.Widget, lowLevelApi.WidgetTask, MU] =
    given LabelPlacement[Measurable[MU, LayoutPlacementMeta[MU]], TextStyle] with
      override def sizeText(text : String, options: TextStyle): Measurable[MU, LayoutPlacementMeta[MU]] = _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].zero)
    given lowLevelApi.type = lowLevelApi

    new HighLevelApiImpl[IO, DrawT[MU], MeasurableT[MU], MU, TextStyle](using lowLevelApi)(drawApi) with LayoutApiImpl[IO, DrawT[MU], lowLevelApi.PlacementEffect, MU](
      [Event] => (axis, elements, main, additional) => weightedRowColumnPlace(
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, mainAxisStrategyPlacement(main, _, _), additionalAxisStrategyPlacement(additional, _, _))
      ).map(unpack)
    ) {
      override val wl: WidgetLibraryImpl[IO, DrawT[MU][TextStyle], MeasurableT[MU]] = lowLevelApi
    }.asInstanceOf[HL[lowLevelApi.Widget, lowLevelApi.WidgetTask, MU]] // TODO do not believe me
  end hegherApi

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler

  def app(using api : HighLevelApi & LayoutApi[MU] & LabelApi[Unit]) : api.Widget[ApplicationRequest]
end Gui4sApp
