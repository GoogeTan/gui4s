package gui4s.android.skia.font

import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

def makeFont[F[_] : Sync as S](typeface : Typeface) : Resource[F, Font] =
  Resource.fromAutoCloseable(
    S.delay(new Font(typeface))
  )
end makeFont

def makeFont[F[_] : Sync as S](typeface : Typeface, size : Float) : Resource[F, Font] =
  Resource.fromAutoCloseable(
    S.delay(new Font(typeface, size))
  )
end makeFont
