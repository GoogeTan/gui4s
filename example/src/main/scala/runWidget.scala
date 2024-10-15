package me.katze.gui4s.example

import draw.*
import place.RunPlacement
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.given
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.stateful.BiMonad

// TODO отрефакторить это сверху вниз по-человечески. 
def runWidget[
  F[+_] : Concurrent,
  UpdateF[+_, +_] : BiMonad, 
  MergeF[+_] : Monad,
  Draw,
  PlacementEffect[+_],
  MU,
  UpEvent,
  DownEvent,
  Widget[A, B] <: EventConsumer[F[Widget[A, B]], F, A, B] & Drawable[Draw]
](
  using lib: WidgetLibraryImpl[UpdateF, MergeF, Draw, PlacementEffect, DownEvent]
)(
    widget                  : Queue[F, DownEvent] => F[Widget[UpEvent, DownEvent]],
    drawLoopExceptionHandler: Throwable => F[Option[ExitCode]],
    api                     : SimpleDrawApi[MU, lib.Draw],
    runDraw                 : lib.Draw => F[Unit]
)(using ProcessRequest[F, UpEvent], RunPlacement[F, PlacementEffect]): F[ExitCode] =
  applicationLoop[F, UpEvent, DownEvent, Widget](
    widget,
    currentWidget => drawLoop[F, Throwable](drawLoopExceptionHandler, runDraw(api.beginDraw), runDraw(api.endDraw))(currentWidget.map(_.draw).flatMap(runDraw)),
    updateLoop
  ).flatMap(_.join)
end runWidget
