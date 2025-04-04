package me.katze.gui4s.example
package update

import cats.effect.ExitCode

enum ApplicationRequest:
  case CloseApp(code : ExitCode)
end ApplicationRequest
