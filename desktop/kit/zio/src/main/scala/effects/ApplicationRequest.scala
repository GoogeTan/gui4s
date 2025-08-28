package gui4s.desktop.kit.zio
package effects

import zio.*

enum ApplicationRequest:
  case CloseApp(code : ExitCode)
end ApplicationRequest
