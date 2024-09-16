package me.katze.gui4s.example
package syntax

import application.*

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.kernel.RefSource
import cats.effect.syntax.all.{*, given}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.PlacedWidget

given drawLoopImpl[
  F[+_] : Monad : Log,
  Draw[+_] : Run_[F],
  E : Show
](using ApplicativeError[Draw, E]) : DrawLoop[F, Draw[Unit]] with
  override def drawLoop(widget: RefSource[F, ? <: PlacedWidget[Draw[Unit], ?, ?, ?, ?]]): F[ExitCode] =
    val error = drawWidget(widget).iterateUntil(_.isEmpty /* Работаем пока у нас нет ошибок*/)
    error.map(_.show).flatMap(logErrorMessage) *> ExitCode.Error.pure[F]
  end drawLoop

  private def drawWidget(ref: RefSource[F, ? <: PlacedWidget[Draw[Unit], ?, ?, ?, ?]]) : F[Option[E]] =
    for
      value <- ref.get
      result : Either[E, Unit] <- run(value.draw.attempt)
    yield result.swap.toOption
  end drawWidget
end drawLoopImpl

