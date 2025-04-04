package me.katze.gui4s.example
package draw

import update.{ApplicationRequest, ProcessRequest}

import cats.*
import cats.effect.*
import cats.syntax.all.given

final class ProcessRequestImpl[F[_] : Applicative] extends ProcessRequest[F, ApplicationRequest]:
  extension (request: ApplicationRequest)
    override def process: F[Option[ExitCode]] =
      request match
        case ApplicationRequest.CloseApp(code)      => Some(code).pure[F]
      end match
    end process
  end extension
end ProcessRequestImpl