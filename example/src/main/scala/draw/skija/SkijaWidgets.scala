package me.katze.gui4s.example
package draw.skija

import api.*
import draw.skija.given
import me.katze.gui4s.example.impl.containerPlacementCurried2
import place.MainAxisStrategyErrors

import cats.syntax.all.*
import cats.{Applicative, Monad, Monoid}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint}
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.{Axis, Measurable, MeasurableT, given}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.TaskFinished
import me.katze.gui4s.widget.{EventResult, given}

type Update[UpEvent] = [Value] =>> EventResult[Value, UpEvent]
type Recomposition[F[_]] = F[Unit]

given monadUnitIsEmpty[F[_]: Applicative as M] : Monoid[Recomposition[F]] with
  override def empty: Recomposition[F] = M.pure(())

  override def combine(x: Recomposition[F], y: Recomposition[F]): Recomposition[F] =
    x *> y
end monadUnitIsEmpty

type PlacedWidget[F[+_], Event] =  widget.Widget[[Value] =>> EventResult[Value, Event], SkijaDraw[F, OglWindow],  MeasurableT[F, Float], Recomposition[F], TaskFinished]
type Widget[F[+_], Event] = Measurable[F, Float,PlacedWidget[F, Event]]

def skijaText[F[+_] : {Monad, Impure}, Window](using backend: SkijaBackend[F, Window])(text : String, font : Font, paint : Paint) : Widget[F, Nothing] =
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

def skijaRow[F[+_] : {Monad, Impure}, Event](using errors: MainAxisStrategyErrors)(
  children : List[Widget[F, Event]],
  horizontalStrategy: MainAxisPlacementStrategy[Float],
  verticalStrategy  : AdditionalAxisPlacementStrategy
): Widget[F, Event] =
  skijaLinearLayout(children, Axis.Vertical, horizontalStrategy, verticalStrategy)
end skijaRow

def skijaColumn[F[+_] : {Monad, Impure}, Event](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event] =
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

