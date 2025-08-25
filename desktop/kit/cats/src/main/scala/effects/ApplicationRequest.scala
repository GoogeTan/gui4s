package gui4s.desktop.kit.cats
package effects

import cats.effect.ExitCode

enum ApplicationRequest:
  case CloseApp(code : ExitCode)
end ApplicationRequest
