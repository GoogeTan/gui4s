package me.katze.gui4s.example
package draw

import update.{ApplicationRequest, ProcessRequest}

import cats.*
import cats.effect.*
import cats.syntax.all.{*, given}


class SwingProcessRequest[F[_] : Applicative](window : SwingApi[F]) extends ProcessRequest[F, ApplicationRequest]:
  extension (request: ApplicationRequest)
    override def process: F[Option[ExitCode]] =
      request match
        case ApplicationRequest.BecomeFullScreen    => window.setSize(None) *> None.pure[F]
        case ApplicationRequest.BecomeNotFullScreen => window.setSize(Some(500, 500)) *> None.pure[F]
        case ApplicationRequest.CloseApp(code)      => Some(code).pure[F]
      end match
    end process
  end extension
end SwingProcessRequest