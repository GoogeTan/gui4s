package gui4s.desktop.kit.zio
package effects

import cats.effect.IO
import gui4s.desktop.skija.SkijaDraw

type Draw = SkijaDraw[IO]