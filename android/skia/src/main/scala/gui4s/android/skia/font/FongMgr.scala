package gui4s.android.skia.font

import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

def makeFontMgr[F[_] : Sync as S] : Resource[F, FontMgr] =
  Resource.fromAutoCloseable(
    S.delay(
      FontMgr.Companion.getDefault
    )
  )
end makeFontMgr
