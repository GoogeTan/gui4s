package gui4s.android.skia.canvas

import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

def clipPath[IO[_] : {Sync, Canvased}](path: Path): IO[Unit] =
  Canvased.applyCanvasFFI(_.clipPath(path))
end clipPath

def withClipedPath[IO[_] : {Sync, Canvased}, T](path: Path, original: IO[T]): IO[T] =
  stateScoped(clipPath(path) *> original)
end withClipedPath