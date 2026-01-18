package gui4s.desktop.skija
package canvas

import cats.effect.kernel.Sync
import cats.syntax.all._
import io.github.humbleui.skija.Path

def clipPath[IO[_] : {Sync, Canvased}](path: Path): IO[Unit] =
  Canvased.applyCanvasFFI(_.clipPath(path))
end clipPath

def withClipedPath[IO[_] : {Sync, Canvased}, T](path: Path, original: IO[T]): IO[T] =
  stateScoped(clipPath(path) *> original)
end withClipedPath