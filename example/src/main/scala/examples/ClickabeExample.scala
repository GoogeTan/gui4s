package me.katze.gui4s.example
package examples

import api.*
import api.effects.SkijaDownEvent.{eventOfferingCallbacks, extractMouseClickEvent}
import api.effects.{*, given}
import api.widget.*
import place.*
import skija.SkijaBackend

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.example.app.skijaGlfwApp
import me.katze.gui4s.geometry.*
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.library
import me.katze.gui4s.widget.library.decorator.*
import scalacache.caffeine.CaffeineCache

object ClickabeExample extends IOApp with ExampleApp:
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]
  val containerPlacementError = ContainerPlacementError.English

  type UpdateError = String
  type PlaceError = String

  override type Update[Event, Value] = SkijaUpdate[IO, Float, SkijaClip, UpdateError, Event, Value]

  type OuterPlace[Value] = SkijaOuterPlace[IO, Rect[Float], PlaceError, Value]
  type InnerPlace[Value] = Sized[Float, Value]

  override type Place[Value] = OuterPlace[InnerPlace[Value]]
  override type Draw = SkijaDraw[IO]
  override type RecompositionReaction = SkijaRecomposition[IO]
  override type DownEvent = SkijaDownEvent[Float]

  type PreInit = (shaper : Shaper, globalTextCache : TextCache[IO])

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(scalacacheCache)
    yield (shaper, cache)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    skijaGlfwApp[
      IO,
      UpdateC[SkijaApplicationRequest],
      Place,
      Draw,
      RecompositionReaction,
      DownEvent,
      PreInit
    ](
      preInit = preInit,
      main = main,
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      settings = WindowCreationSettings(
        title = "Gui4s image widget example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
      ffi = ffi,
      callbacks = sink => SkijaDownEvent.eventOfferingCallbacks(sink.offer),
      runUpdate = SkijaUpdate.handleApplicationRequests[IO, Float, SkijaClip, String](error => IO.println(error).as(ExitCode.Error)),
      runPlace = backend => SkijaPlace.run[IO, Rect[Float], Float, PlaceError](backend.windowBounds).andThen[EitherT[IO, Throwable, *]](eitherTMapError[IO, String, Throwable](new Exception(_))).andThen(runEitherT[IO, Throwable]),
      runDraw = (draw, backend) => backend.drawFrame(ffi, (clear[IO] |+| draw).run),
      runRecomposition = SkijaRecomposition.run[IO]
    )
  end run

  def main(preInit : PreInit, backend : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Widget[SkijaApplicationRequest] =
    def updateDecorator[Event]: UpdateDecorator[
      UpdateC[Event],
      OuterPlace,
      InnerPlace[PlacedWidget[Event]],
      DownEvent
    ] = updateDecoratorWithRect[
      UpdateC[Event], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent
    ]

    def eventCatcher[Event]: EventCatcherWithRect[
      Widget[Event],
      Update[Event, Boolean],
      InnerPlace[PlacedWidget[Event]],
      DownEvent
    ] = eventCatcherWithRect[
      PlacedWidget[Event],
      UpdateC[Event],
      OuterPlace,
      InnerPlace,
      DownEvent
    ](
      updateDecorator,
      SkijaUpdate.markEventHandled,
      widgetAsFree,
      widgetHandlesEvent
    )

    extension[Event](value : Widget[Event])
      def onClick(event : Event) : Widget[Event] =
        clickCatcher(
          eventCatcherWithRect = eventCatcher,
          currentMousePosition = SkijaUpdate.liftF(backend.mousePosition),
          approprieteEvent = extractMouseClickEvent,
          onClick = (_, _) => SkijaUpdate.raiseEvents[IO, Float, SkijaClip, UpdateError, Event](List(event)).as(true),
          isIn = point => shape =>
            SkijaUpdate.getCoordinates2d[IO, Float, SkijaClip, UpdateError, Event].map(
              coordinatesOfTopLeftCornet =>
                RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet).containsPoint(point)
            )
        )(value)
      end onClick
    end extension

    def statefulWidget: StatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]] = skijaStateful(
      (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
    )

    def text[Event](text : String, style : SkijaTextStyle) : Widget[Event] =
      me.katze.gui4s.widget.library.text[
        UpdateC[Event],
        Place,
        SkijaDraw[IO],
        RecompositionReaction,
        DownEvent,
        SkijaPlacedText
      ](
        SkijaPlace.sizeText[IO, Rect[Float], PlaceError](ffi, preInit.shaper, preInit.globalTextCache, _.width.some)(text, style),
        drawText(ffi, _),
        Monoid[RecompositionReaction].empty,
      )
    end text

    def container[Container[_] : Traverse, Event](
                                                    updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
                                                  ) : ContainerWidget[PlacedWidget[Event], Container, Place, Point3d[Float]] =
      given Order[Point3d[Float]] = Order.by(_.z)
      library.container(
        (draw, meta) => drawAt(ffi, draw, meta.x, meta.y),
        [T] => (update, point) => SkijaUpdate.withCoordinates(update)(_ + point),
        SkijaUpdate.isEventHandled[IO, Float, SkijaClip, UpdateError, Event],
        updateListOrdered
      )
    end container

    def linearContainer[Event] : LinearContainer[Widget[Event], OuterPlace, List, Float, Float, Axis] =
      library.linearContainer[
        PlacedWidget[Event],
        OuterPlace,
        List,
        Float,
        Float,
      ](
        container = container([A : Order, B] => v => f => orderedListProcessing(v)(f)),
        getBounds = SkijaOuterPlace.getBounds,
        setBounds = SkijaOuterPlace.setBounds,
        cut = _ - _
      )
    end linearContainer

    def clickExample[Event](numbers : List[Int]): Widget[Event] =
      linearContainer(
        mainAxis = Axis.Vertical,
        mainAxisStrategy = ManyElementsPlacementStrategy.Begin[OuterPlace, Float, List, Float](0f),
        additionalAxisStrategy = OneElementPlacementStrategy.Center,
        children = numbers.map:
          lineNumber =>
            statefulWidget[Int, Event, Unit](
              name = "line-" + lineNumber.toString,
              initialState = 0,
              eventHandler = (state, _, _) => (state + 1).pure[SkijaUpdateT[IO, Float, SkijaClip, String, Event]],
              body = state =>
                text(
                  "# " + lineNumber.toString + " : " + state.toString,
                  SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
                ).onClick(())
            ),
      )
    end clickExample

    clickExample((0 until 10).toList)
  end main
end ClickabeExample