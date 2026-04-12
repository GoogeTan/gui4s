package gui4s.desktop.windowing.jwm

import cats.*
import cats.effect.*
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import gui4s.core.geometry.{InfinityOr, Rect}
import gui4s.core.loop.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.processEvent
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.widgets.{DesktopPlacedWidget, DesktopWidget}
import gui4s.desktop.widget.library.*
import io.github.humbleui.jwm.*
import io.github.humbleui.jwm.skija.{EventFrameSkija, LayerMetalSkija}

trait UIApp:
  given runtime: IORuntime = IORuntime.global

  def appResource(
    window: Window,
    windowBounds : IO[Bounds]
  ): Resource[IO, (Queue[IO, DownEvent], Ref[IO, DesktopPlacedWidget[Nothing]])] = for
    eventBus <- Resource.eval(Queue.unbounded[IO, DownEvent])

    runPlace = Place.run(Path(Nil), windowBounds)
    loop = makeUpdateLoop(runPlace)

    widget <- Init.runWidget(main(window, eventBus)).evalMap(runWidgetForTheFirstTime(_, runPlace, RecompositionReaction.run))
    widgetCell <- Resource.eval(Ref.of[IO, DesktopPlacedWidget[Nothing]](widget))

    _ <- Resource.eval(
      loop(
        widget,
        newWidget => widgetCell.set(newWidget) <* IO.delay(window.requestFrame()),
        eventBus.tryTakeN(None)
      ).start
    )

  yield (eventBus, widgetCell)

  def layerBounds(layer : Layer): IO[Bounds] =
    IO.delay(Rect(InfinityOr(Some(layer.getWidth.toFloat)), InfinityOr(Some(layer.getHeight.toFloat))))
  end layerBounds

  def makeUpdateLoop(runPlace : Place ~> IO): UpdateLoop[IO, DesktopPlacedWidget[Nothing], List[DownEvent], ExitCode] =
    updateLoop(
      (widget, event) =>
        flattenRight(
          Update.runUpdate(
            processEvent(
              Path(Nil),
              RecompositionReaction.run,
              widgetAsFree,
              widgetHandlesEvent[Update[Nothing, *], Place, Draw, RecompositionReaction],
              widgetReactsOnRecomposition,
              widgetHasInnerStates,
              runPlace,
              widget
            ),
            event
          )
        )
    )
  end makeUpdateLoop

  def main(
    window: Window,
    eventBus: Queue[IO, DownEvent],
  ): Init[DesktopWidget[Nothing]]

  def main(args: Array[String]): Unit =
    App.start(() =>
      val window = App.makeWindow()
      val layer = new LayerMetalSkija()
      window.setLayer(layer)

      val ((eventBus, widgetCell), cleanupIO) = appResource(window, layerBounds(layer)).allocated.unsafeRunSync()

      window.setEventListener:
        case _: EventWindowCloseRequest =>
          cleanupIO.unsafeRunSync()
          window.close()
          App.terminate()

        case resize: EventWindowResize =>
        case event: EventFrameSkija =>
          val surface = event.getSurface
          val canvas = surface.getCanvas
          val widget = widgetCell.get.unsafeRunSync()
          canvas.clear(0xFFFFFFFF)
          widgetIsDrawable(widget).run(canvas).unsafeRunSync()
        case someAnotherEvent =>
          eventBus.offer(UserEvent(someAnotherEvent)).unsafeRunAndForget()
      window.setVisible(true)
    )
end UIApp