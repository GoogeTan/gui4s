package gui4s.desktop.example.glfw

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.PurePostInit
import glfw4s.jna.bindings.structs
import glfw4s.jna.bindings.types.{GLFWmonitor, GLFWwindow}
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.widget.library.decorator.Paddings
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.decorator.{clickCatcher as _, *}
import gui4s.desktop.kit.widgets.{scrollWidget as _, *}
import gui4s.desktop.skija.typeface.defaultTypeface
import gui4s.desktop.skija.{Brush, Paint, SkijaTextStyle, StrokeOptions}
import gui4s.desktop.windowing.glfw.*
import io.github.humbleui.skija.Font
import gui4s.desktop.example.shared.*

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
        Init.run(scenes(glfw, window, eventBus, onClick))
      ):
        case Some(baseDecorations, loadedScenes) =>
          baseDecorations(
            statefulWidget[Int, Nothing, Int](
              name = "currentScene",
              initialState = 0,
              eventHandler = (state : Int, events : List[Int]) =>
                ((state + events.sum + loadedScenes.length) % loadedScenes.length).pure[UpdateC[Nothing]],
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
          Shape.roundedCorners(50f),
        )
        .border(
          Shape.roundedCorners(50f),
          Brush.solid(0xFF454649),
          StrokeOptions()
        )
        .onClick(event)
  end button

  def scenes(
    glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, structs.GLFWcursor, Int],
    window: GLFWwindow,
    eventBus : Queue[IO, DownEvent],
    clickCatcher : ClickCatcher,
  ) : Init[NonEmptyList[DesktopWidget[Nothing]]] =
    for
      textWidget <- TextWidget()
      resourceWidget <- ResourceWidget(eventBus)
      initializationWidget = InitializationWidget(resourceWidget)
      text <- TextWidget()
      typeface <- Init.evalResource(defaultTypeface[IO])
      scroll <- scrollWidget(glfw, window, eventBus)
    yield NonEmptyList.of(
      animationExample(initializationWidget, text, typeface)(using scroll),
      imageExample(text, typeface, initializationWidget),
      gridExample(text, typeface),
      statefulExample(text, typeface)(using clickCatcher)
    )
  end scenes
end ScenedExample
