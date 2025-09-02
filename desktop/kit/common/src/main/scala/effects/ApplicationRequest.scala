package gui4s.desktop.kit
package common.effects

import cats.effect.ExitCode

enum ApplicationRequest:
  case CloseApp(code : ExitCode)
end ApplicationRequest
