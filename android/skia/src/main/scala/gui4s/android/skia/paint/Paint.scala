package gui4s.android.skia.paint

import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

def make[F[_] : Sync as S] : Resource[F, Paint] =
  Resource.fromAutoCloseable(S.delay(new Paint()))
end make

def setColour[F[_] : Sync as S](color : Int)(paint : Paint) : F[Unit] =
  S.delay(paint.setColor(color))
end setColour

