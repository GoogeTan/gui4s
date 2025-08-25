package gui4s.desktop.example.cats

import catnip.effect.SyncForeignFunctionInterface
import catnip.syntax.all.{*, given}
import catnip.{BiMonad, ForeignFunctionInterface}
import cats.*
import cats.data.*
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import gui4s.core.geometry.*
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import gui4s.desktop.kit.cats.*
import gui4s.desktop.kit.cats.widgets.*
import gui4s.desktop.kit.cats.effects.{*, given}
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import gui4s.desktop.skija.*
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.{Path, StatefulState}
import scalacache.caffeine.CaffeineCache
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library.decorator.*

import scala.reflect.Typeable

object ClickabeExample extends IOApp:
  given ffi : ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]

  type PreInit = (shaper : Shaper, globalTextCache : TextCache[IO], mousePosition : IO[Point2d[Float]])

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(ScalacacheCache(_))
    yield (shaper, cache, backend.mousePosition)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    desktopApp(
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
    )
  end run

  def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
    def updateDecorator[Event]: UpdateDecorator[
      UpdateC[Event],
      OuterPlace,
      InnerPlace[DesktopPlacedWidget[Event]],
      DownEvent
    ] = updateDecoratorWithRect[
      UpdateC[Event], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent
    ]

    def eventCatcher[Event]: EventCatcherWithRect[
      DesktopWidget[Event],
      Update[Event, Boolean],
      InnerPlace[DesktopPlacedWidget[Event]],
      DownEvent
    ] = eventCatcherWithRect[
      DesktopPlacedWidget[Event],
      UpdateC[Event],
      OuterPlace,
      InnerPlace,
      DownEvent
    ](
      updateDecorator,
      Update.markEventHandled,
      widgetAsFree,
      widgetHandlesEvent
    )

    extension[Event](value : DesktopWidget[Event])
      def onClick(event : Event) : DesktopWidget[Event] =
        clickCatcher(
          eventCatcherWithRect = eventCatcher,
          currentMousePosition = Update.liftK(preInit.mousePosition),
          approprieteEvent = DownEvent.extractMouseClickEvent,
          onClick = (_, _) => Update.emitEvents(List(event)).as(true),
          isIn = point => shape =>
            Update.getCornerCoordinates.map(
              coordinatesOfTopLeftCornet =>
                RectAtPoint2d(shape.size, coordinatesOfTopLeftCornet.projectToXY).containsPoint(point)
            )
        )(value)
      end onClick
    end extension

    def statefulWidget : StatefulWidget[DesktopWidget, Update, [State] =>> State => RecompositionReaction] =
      new StatefulWidget[DesktopWidget, Update,  [State] =>> State => RecompositionReaction]:
        override def apply[State: Typeable, Event, ChildEvent](
                                                                name: String,
                                                                initialState: State,
                                                                eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                                body: State => DesktopWidget[ChildEvent]
                                                              ): DesktopWidget[Event] =
          apply(name, initialState, eventHandler, body, _ => SkijaRecomposition.empty[IO])
        end apply

        override def apply[State: Typeable, Event, ChildEvent](
                                                                name: String,
                                                                initialState: State,
                                                                eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                                body: State => DesktopWidget[ChildEvent],
                                                                destructor: State => RecompositionReaction[IO]
                                                              ): DesktopWidget[Event] =
          library.stateful[
            UpdateC[Event],
            UpdateC[ChildEvent],
            Place,
            Draw,
            RecompositionReaction,
            DownEvent,
            State,
            ChildEvent
          ](
            widgetsAreMergeable = widgetsAreMergable[UpdateC[ChildEvent], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent],
            typeCheckState = Place.typecheck((value : Any, path : Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"),
            liftUpdate = Update.catchEvents
          )(
            name = name,
            initialState = initialState,
            handleEvent = eventHandler,
            render = body,
            destructor = destructor
          )
        end apply
      end new
    end statefulWidget

    def text[Event](text : String, style : SkijaTextStyle) : DesktopWidget[Event] =
      me.katze.gui4s.widget.library.text[
        UpdateC[Event],
        Place,
        SkijaDraw[IO],
        RecompositionReaction,
        DownEvent,
        SkijaPlacedText
      ](
        Place.sizeText(ffi, preInit.shaper, preInit.globalTextCache, _.width.some)(text, style),
        drawText(ffi, _),
        Monoid[RecompositionReaction].empty,
      )
    end text

    def container[Container[_] : Traverse, Event](
                                                    updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
                                                  ) : ContainerWidget[DesktopPlacedWidget[Event], Container, Place, Point3d[Float]] =
      given Order[Point3d[Float]] = Order.by(_.z)
      library.container(
        (draw, meta) => drawAt(ffi, draw, meta.x, meta.y),
        [T] => (update : Update[Event, T], point : Point3d[Float]) => Update.withCornerCoordinates(update, _ + point),
        Update.isEventHandled,
        updateListOrdered
      )
    end container

    def linearContainer[Event] : LinearContainer[DesktopWidget[Event], OuterPlace, List, Float, Float, Axis] =
      library.linearContainer[
        DesktopPlacedWidget[Event],
        OuterPlace,
        List,
        Float,
        Float,
      ](
        container = container([A : Order, B] => v => f => orderedListProcessing(v)(f)),
        getBounds = OuterPlace.getBounds,
        setBounds = OuterPlace.setBounds,
        cut = _ - _
      )
    end linearContainer

    def clickExample[Event](numbers : List[Int]): DesktopWidget[Event] =
      linearContainer(
        mainAxis = Axis.Vertical,
        mainAxisStrategy = ManyElementsPlacementStrategy.Begin[OuterPlace, Float, List, Float](0f),
        additionalAxisStrategy = OneElementPlacementStrategy.Center,
        children = numbers.map:
          lineNumber =>
            statefulWidget[Int, Event, Unit](
              name = "line-" + lineNumber.toString,
              initialState = 0,
              eventHandler = (state, _, _) => (state + 1).pure[UpdateC[Event]],
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
