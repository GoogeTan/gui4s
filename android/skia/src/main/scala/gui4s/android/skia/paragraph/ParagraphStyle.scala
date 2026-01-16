package gui4s.android.skia.paragraph

import org.jetbrains.skia.paragraph.*
import cats.effect.*

def makeParagraphStyle[F[_] : Sync as S] : Resource[F, ParagraphStyle] =
  Resource.fromAutoCloseable(
    S.delay(
      new ParagraphStyle
    )
  )
end makeParagraphStyle

