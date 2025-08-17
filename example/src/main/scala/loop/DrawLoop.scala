package me.katze.gui4s.example
package loop

import catnip.syntax.monad.MonadErrorT
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.{Async, Concurrent, ExitCode, Ref}
import cats.syntax.all.given
import cats.{ApplicativeError, Monad, MonadError}

import scala.concurrent.ExecutionContext

/**
 * Принимает способ получить нынешнее дерево виджетов и возвращает бесконечный цикл отрисовки. Завершается только в случае ошибки.
 */
type DrawLoop[F[+_], -Widget] = F[Widget] => F[ExitCode]

def runDrawLoopOnExecutionContext[F[+_]: Async, Widget](loop : DrawLoop[F, Widget], context : ExecutionContext) : DrawLoop[F, Widget] =
  widgetSource => loop(widgetSource).evalOn(context)
end runDrawLoopOnExecutionContext

type DrawLoopExceptionHandler[F[_], Error] = Error => F[Option[ExitCode]]

def drawLoop[
  F[_] : MonadErrorT[Error],
  Error
](renderExceptionHandler : DrawLoopExceptionHandler[F, Error], shouldContinue : F[Boolean])(drawCurrentWidget : F[Unit]) : F[Option[ExitCode]] =
  runWhileNoError(
    drawCurrentWidget,
    renderExceptionHandler,
    shouldContinue
  )
end drawLoop


def runWhileNoError[F[_] : MonadErrorT[InternalError], InternalError, ExternalError](
                                                                                      effect: F[Unit],
                                                                                      recover: InternalError => F[Option[ExternalError]], 
                                                                                      shouldContinue: F[Boolean]
                                                                                    ): F[Option[ExternalError]] =
  Monad[F].tailRecM[None.type, Option[ExternalError]](
    None
  )(_ =>
    shouldContinue.flatMap(continue =>
      if continue then
        effect
          .as(Left(None))
          .handleErrorWith(error => recover(error).map(_.toRight(None).map(Some(_))))
      else
        Right(None).pure[F]
    )
  )
end runWhileNoError
