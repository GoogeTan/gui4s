package gui4s.android.skia.paragraph

import org.jetbrains.skia.paragraph.*
import org.jetbrains.skia.*

def makeTextStyle[F[_] : Sync as S] : Resource[F, TextStyle] =
  Resource.fromAutoCloseable(
    S.delay(new TextStyle)
  )
end makeTextStyle

def textStyleModifier[F[_] : Sync as S](f : TextStyle => Unit) : TextStyle => F[Unit] =
  style => S.delay(f(style))
end textStyleModifier

def setTypeface[F[_] : Sync](typeface: Typeface) : TextStyle => F[Unit] =
  textStyleModifier(_.setTypeface(typeface))
end setTypeface

def setFontSize[F[_] : Sync](size : Float) : TextStyle => F[Unit] =
  textStyleModifier(_.setFontSize(size))
end setFontSize

def setForeground[F[_] : Sync](paint : Paint) : TextStyle => F[Unit] =
  textStyleModifier(_.setForeground(paint))
end setForeground

def setBackground[F[_] : Sync](paint : Paint) : TextStyle => F[Unit] =
  textStyleModifier(_.setBackground(paint))
end setBackground