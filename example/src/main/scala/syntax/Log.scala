package me.katze.gui4s.example
package syntax

trait Log[F[_]]:
  def logError(line: Throwable): F[Unit]

  def logErrorMessage(line: String) : F[Unit]

  def logWarning(line: String): F[Unit]

  def debug(line: String): F[Unit]
end Log

def logError[F[_]](line: Throwable)(using log: Log[F]): F[Unit] = log.logError(line)

def logErrorMessage[F[_]](line: String)(using log: Log[F]): F[Unit] = log.logErrorMessage(line)

def logWarning[F[_]](line: String)(using log: Log[F]): F[Unit] = log.logWarning(line)

def debug[F[_]](line: String)(using log : Log[F]): F[Unit] = log.debug(line)