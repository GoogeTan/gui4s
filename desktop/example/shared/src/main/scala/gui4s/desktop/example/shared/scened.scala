package gui4s.desktop.example.shared

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.std.Queue
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.widget.library.decorator.Paddings
import gui4s.desktop.kit.effects.{DownEvent, Init, LinearContainerPlacementStrategy, Shape, UpdateC}
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.{Brush, Paint, SkijaTextStyle, StrokeOptions}
import io.github.humbleui.skija.Font
import gui4s.desktop.skija.typeface.defaultTypeface

def scenedExample[ScrollEvent](
  using
    clickCatcher: ClickCatcher,
    scrollEventSource: ScrollEventSource[ScrollEvent],
    eventBus: Queue[IO, DownEvent]
): Init[DesktopWidget[Nothing]] =
  for
    text <- TextWidget()
    resourceWidget <- ResourceWidget(eventBus)
    typeface <- Init.evalResource(defaultTypeface[IO])
    basicStyle = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
  yield
    resourceWidget(
      "scenes",
      Init.run(scenes(resourceWidget, text))
    ):
      case Some(baseDecorations, loadedScenes) =>
        baseDecorations(
          statefulWidget[Int, Nothing, Int](
            name = "currentScene",
            initialState = 0,
            eventHandler = (state : Int, events : List[Int]) =>
              ((state + events.sum + loadedScenes.length) % loadedScenes.length).pure[UpdateC[Nothing]],
            body = state =>
              sceneController(
                loadedScenes.toList(state).mapEvent(identity),
                button(text("Далее", basicStyle), 1),
                button(text("Назад", basicStyle), -1)
              )
          )
        )
      case None =>
        text("Загрузка...", basicStyle)
end scenedExample

def sceneController[Event](
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
end sceneController

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

def scenes[ScrollEvent](
  resourceWidget: ResourceWidget,
  textWidget: TextWidget
)(
  using ScrollEventSource[ScrollEvent], ClickCatcher,
) : Init[NonEmptyList[DesktopWidget[Nothing]]] =
  for
    initializationWidget = InitializationWidget(resourceWidget)
    text <- TextWidget()
    typeface <- Init.evalResource(defaultTypeface[IO])
    scroll = scrollWidget(summon, animationWidget)
  yield NonEmptyList.of(
    animationExample(initializationWidget, text, typeface)(using scroll),
    imageExample(text, typeface, initializationWidget),
    gridExample(text, typeface),
    statefulExample(text, typeface)
  )
end scenes

