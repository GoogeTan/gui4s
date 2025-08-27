package gui4s.desktop.kit.zio
package effects

import cats.effect.ExitCode

enum ApplicationRequest:
  case CloseApp(code : ExitCode)
end ApplicationRequest
