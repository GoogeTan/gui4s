package gui4s.desktop.example

import cats.data.NonEmptyList
import cats.effect.std.Queue
import cats.effect.IO
import cats.syntax.all.*
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.PurePostInit
import glfw4s.jna.bindings.structs
import glfw4s.jna.bindings.types.{GLFWmonitor, GLFWwindow}
import gui4s.core.layout.rowcolumn.OneElementPlacementStrategy
import gui4s.desktop.kit.UIApp
import gui4s.desktop.kit.effects.{DownEvent, Init, LinearContainerPlacementStrategy, Shapes, UpdateC}
import gui4s.desktop.skija.typeface.defaultTypeface
import gui4s.desktop.skija.{Brush, Paint, SkijaTextStyle, StrokeOptions}
import io.github.humbleui.skija.Font
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.decorator.padding
import gui4s.core.widget.library.decorator.Paddings

object ScenedExample extends UIApp:
  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s animation example",
    width = 620,
    height = 480,
  )

  override def main(
    glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, structs.GLFWcursor, Int],
    window: GLFWwindow,
    eventBus: Queue[IO, DownEvent]
  ): Init[DesktopWidget[Nothing]] =
    for
      text <- TextWidget()
      resourceWidget <- ResourceWidget(eventBus)
      typeface <- Init.evalResource(defaultTypeface[IO])
      onClick  <- clickCatcher(window, glfw, eventBus)
      given ClickCatcher = onClick
      basicStyle = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
    yield
      resourceWidget(
        "scenes",
        Init.run(scenes(eventBus, onClick))
      ):
        case Some(baseDecorations, loadedScenes) =>
          baseDecorations(
            statefulWidget[Int, Nothing, Int](
              name = "currentScene",
              initialState = 0,
              eventHandler = (state : Int, _, events : NonEmptyList[Int]) =>
                ((state + events.toList.sum + loadedScenes.length) % loadedScenes.length).pure[UpdateC[IO, Nothing]],
              body = state =>
                scener(
                  loadedScenes.toList(state).mapEvent(identity),
                  button(text("Далее", basicStyle), 1),
                  button(text("Назад", basicStyle), -1)
                )
            )
          )
        case None =>
          text("Загрузка...", basicStyle)
  end main

  def scener[Event](
    body : DesktopWidget[Event],
    goFurther : DesktopWidget[Event],
    goBack : DesktopWidget[Event]
  ) : DesktopWidget[Event] =
    boxWidget(
      body
    ).withForeground(
      rowWidget(
        List(
          goFurther,
          goBack
        ),
        horizontalPlacementStrategy = LinearContainerPlacementStrategy.Begin[List](10f)
      ),
      horizontalPlacementStrategy  = OneElementPlacementStrategy.Begin,
      verticalPlacementStrategy    = OneElementPlacementStrategy.Begin,
    )
  end scener

  def button(
              using onClick : ClickCatcher,
             ): [Event] =>(
                body : DesktopWidget[Event],
                event : Event
              ) => DesktopWidget[Event] =
    [Event] => (body : DesktopWidget[Event], event : Event) =>
      body
        .padding(Paddings(10f, 10f, 10f, 10f))
        .paintOnBackgroundWith(
          Brush.solid(0xFFCCCCCC),
          Shapes.roundedCorners(50f),
        )
        .border(
          Shapes.roundedCorners(50f),
          Brush.solid(0xFF454649),
          StrokeOptions()
        )
        .onClick(event)
  end button

  def scenes(
    eventBus : Queue[IO, DownEvent],
    clickCatcher : ClickCatcher,
  ) : Init[NonEmptyList[DesktopWidget[Nothing]]] =
    for
      textWidget <- TextWidget()
      resourceWidget <- ResourceWidget(eventBus)
      initializationWidget = InitializationWidget(resourceWidget)
      animation <- animationWidget[Unit, Float](eventBus)
      text <- TextWidget()
      typeface <- Init.evalResource(defaultTypeface[IO])
    yield NonEmptyList.of(
      animationExample(initializationWidget, clickCatcher, animation, text, typeface),
      imageExample(text, typeface, initializationWidget),
      gridExample(text, typeface),
      statefulExample(text, clickCatcher, typeface)
    )
  end scenes
end ScenedExample
