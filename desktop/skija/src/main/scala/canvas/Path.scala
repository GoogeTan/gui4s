package gui4s.desktop.skija
package canvas

import catnip.ForeignFunctionInterface
import cats.FlatMap
import io.github.humbleui.skija.Path
import cats.syntax.all.*

def clipPath[IO[_] : {FlatMap, ForeignFunctionInterface, Canvased}](path: Path): IO[Unit] =
  Canvased.applyCanvasFFI(_.clipPath(path))
end clipPath

def withClipedPath[IO[_] : {FlatMap, ForeignFunctionInterface, Canvased}, T](path: Path, original: IO[T]): IO[T] =
  stateScoped(clipPath(path) *> original)
end withClipedPath