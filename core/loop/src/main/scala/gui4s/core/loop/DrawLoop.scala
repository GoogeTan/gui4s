package gui4s.core.loop

import scala.concurrent.ExecutionContext

import cats.Monad
import cats.effect.Async
import cats.effect.syntax.all._
import cats.syntax.all._

/**
 * Принимает способ получить нынешнее дерево виджетов и возвращает бесконечный цикл отрисовки. Завершается только в случае ошибки.
 */
type DrawLoop[F[_], Widget, ExitCode] = F[Widget] => F[ExitCode]

def runDrawLoopOnExecutionContext[F[_]: Async, Widget, ExitCode](loop : DrawLoop[F, Widget, ExitCode], context : ExecutionContext) : DrawLoop[F, Widget, ExitCode] =
  widgetSource => loop(widgetSource).evalOn(context)
end runDrawLoopOnExecutionContext

def drawLoop[
  F[_] : Monad as M,
  ExitCode
](success : ExitCode) : DrawLoop[F, Boolean, ExitCode] =
  draw =>
    M.iterateWhile(draw)(identity).as(success)
end drawLoop
