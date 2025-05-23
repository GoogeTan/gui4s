package me.katze.gui4s.example
package draw.skija

import api.*
import draw.skija.given
import impl.containerPlacementCurried2
import place.MainAxisStrategyErrors

import catnip.syntax.all.given
import cats.Monad
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint}
import me.*
import me.katze.gui4s.example.EventResult
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.{Axis, Measurable, MeasurableT, given}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.given

type Update[UpEvent] = [Value] =>> EventResult[Value, UpEvent]
type Recomposition[F[_]] = F[Unit]



type PlacedWidget[F[+_], Event, DownEvent] =  SkijaWidget_[[Value] =>> EventResult[Value, Event],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], +Event, -DownEvent] = Measurable[F, Float,PlacedWidget[F, Event, DownEvent]]

def skijaText[F[+_] : {Monad, Impure}, Window](using backend: SkijaBackend[F, Window])(text : String, font : Font, paint : Paint) : Widget[F, Nothing, Any] =
  textWidget[
    EventResult,
    SkijaDraw[F, OglWindow],
    MeasurableT[F, Float],
    Recomposition[F],
    Float,
    SkijaTextStyle,
    SkijaPlacedText,
    Shaper,
    TaskFinished
  ](
    text,
    backend.globalShaper,
    SkijaTextStyle(font, paint)
  )
end skijaText


// TODO Remove using errors

def skijaRow[F[+_] : {Monad, Impure}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Event, DownEvent]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout(children, Axis.Vertical, horizontalStrategy, verticalStrategy)
end skijaRow

def skijaColumn[F[+_] : {Monad, Impure}, Event, DownEvent](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event, DownEvent]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event, DownEvent] =
  skijaLinearLayout(children, Axis.Vertical, verticalStrategy, horizontalStrategy)
end skijaColumn

def skijaLinearLayout[F[+_] : {Monad, Impure}](using errors : MainAxisStrategyErrors) : LinearLayout[[Event] =>> Widget[F, Event], Float, Axis] =
  linearLayout[
    EventResult,
    SkijaDraw[F, OglWindow],
    MeasurableT[F, Float],
    Recomposition[F],
    Float,
    LayoutPlacementMeta[Float],
    TaskFinished,
    Axis
  ](
    containerPlacementCurried2[F, [Event] =>> PlacedWidget[F, Event], Float](errors)
  )
end skijaLinearLayout

