package me.katze.gui4s.example
package update

import cats.effect.ExitCode

trait ProcessRequest[+F[_], -UpEvent]:
  extension (request: UpEvent)
    def process: F[Option[ExitCode]]
  end extension
end ProcessRequest
