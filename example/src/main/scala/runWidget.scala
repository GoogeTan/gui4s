package me.katze.gui4s.example

import draw.*
import place.RunPlacement
import task.TaskSet
import update.*

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.{EventResult, PlacedWidget}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.stateful.Path

// TODO отрефакторить это сверху вниз по-человечески. 
def runWidget[
  F[+_] : Concurrent,
  Draw,
  PlacementEffect[+_], WidgetTask[+_],
  MU,
  UpEvent,
  DownEvent,
  Widget[A, B] <: EventConsumer[F[Widget[A, B]], F, A, B] & Drawable[Draw]
](
  using lib: WidgetLibraryImpl[F, Draw, PlacementEffect, WidgetTask, DownEvent]
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
