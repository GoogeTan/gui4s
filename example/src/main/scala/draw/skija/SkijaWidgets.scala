package me.katze.gui4s.example
package draw.skija

import api.*
import api.impl.*
import draw.skija.{*, given}
import cats.syntax.all.*
import place.MainAxisStrategyErrors

import cats.{Applicative, Monad, Monoid}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint}
import me.katze.gui4s.example.given
import me.katze.gui4s.example.impl.containerPlacementCurried2
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.{Axis, Measurable, MeasurableT, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{Empty, given}
import me.katze.gui4s.widget.stateful.TaskFinished
import me.katze.gui4s.widget.{EventResult, given}

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
  skijaLayoutApi.row(children, horizontalStrategy, verticalStrategy)
end skijaRow

def skijaColumn[F[+_] : {Monad, Impure}, Event](using errors: MainAxisStrategyErrors)(
  children: List[Widget[F, Event]],
  verticalStrategy: MainAxisPlacementStrategy[Float],
  horizontalStrategy: AdditionalAxisPlacementStrategy
): Widget[F, Event] =
  skijaLayoutApi.column(children, verticalStrategy, horizontalStrategy)
end skijaColumn

def skijaLayoutApi[F[+_] : {Monad, Impure}](using errors : MainAxisStrategyErrors) : LayoutApi[[Event] =>> Widget[F, Event], Float] =
  LayoutApi_[
    EventResult, SkijaDraw[F, OglWindow],  MeasurableT[F, Float], Recomposition[F], Float, LayoutPlacementMeta[Float], TaskFinished
  ](
    containerPlacementCurried2[F, [Event] =>> PlacedWidget[F, Event], Float](errors)
  )
end skijaLayoutApi

