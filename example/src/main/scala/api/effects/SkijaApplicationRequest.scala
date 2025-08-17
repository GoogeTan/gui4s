package me.katze.gui4s.example
package api.effects

import cats.effect.ExitCode

enum SkijaApplicationRequest:
  case CloseApp(code : ExitCode)
end SkijaApplicationRequest
