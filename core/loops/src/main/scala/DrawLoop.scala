package gui4s.core.loops

import catnip.syntax.monad.MonadErrorT
import cats.Monad
import cats.effect.syntax.all.*
import cats.effect.{Async, ExitCode}
import cats.syntax.all.given

import scala.concurrent.ExecutionContext

/**
 * Принимает способ получить нынешнее дерево виджетов и возвращает бесконечный цикл отрисовки. Завершается только в случае ошибки.
 */
type DrawLoop[F[_], Widget] = F[Widget] => F[ExitCode]

def runDrawLoopOnExecutionContext[F[_]: Async, Widget](loop : DrawLoop[F, Widget], context : ExecutionContext) : DrawLoop[F, Widget] =
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
  Monad[F].tailRecM[Unit, Option[ExternalError]](
    ()
  )(_ =>
    shouldContinue.flatMap(continue =>
      if continue then
        effect
          .as(Left[Unit, Option[ExternalError]](()))
          .handleErrorWith(error => recover(error).map(_.some.toRight(())))
      else
        Right[Unit, Option[ExternalError]](None).pure[F]
    )
  )
end runWhileNoError
