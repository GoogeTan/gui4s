package me.katze.gui4s.draw
package freetype

import cats.*
import cats.data.EitherT
import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.util.freetype.*
import org.lwjgl.util.freetype.FreeType.*

final class FreeTypeImpl[F[_] : Sync](
                                    makeF : [T] => (() => (Int, () => T)) => F[T],
                                    makeUnfailurable : [T] =>  (() => T) => F[T],
                                   ) extends FontLibrary[F]:
  override type Font = FT_Face
  override type Library = Long

  def stackPushR : Resource[F, MemoryStack] =
    Resource.fromAutoCloseable(makeUnfailurable(() => MemoryStack.stackPush()))
  end stackPushR

  def initLibrary : Resource[F, Long] =
    Resource.make(
      stackPushR.use:
        stack =>
          makeF(() =>
            val pp = stack.mallocPointer(1)
            val error = FT_Init_FreeType(pp)
            (
              error,
              () => pp.get(0)
            )
          )
    )(lib =>
      makeUnfailurable(() => FT_Done_FreeType(lib))
    )
  end initLibrary

  def libraryVersion(lib : Long) : F[(Int, Int, Int)] =
    stackPushR.use:
      stack =>
        makeUnfailurable(() =>
          val major = stack.mallocInt(1)
          val minor = stack.mallocInt(1)
          val patch = stack.mallocInt(1)
          FT_Library_Version(lib, major, minor, patch)
          (major.get(0), minor.get(0), patch.get(0))
        )
  end libraryVersion

  def loadFont(lib : Long, path : CharSequence): Resource[F, FT_Face] =
    Resource.make(
      stackPushR.use:
        stack =>
          makeF(() =>
            val face = stack.mallocPointer(1)
            val error = FT_New_Face(lib, path, 0, face)
            (
              error,
              () => FT_Face.createSafe(face.get(0))
            )
          )
    )(face =>
      makeUnfailurable(() => FT_Done_Face(face))
    )
  end loadFont

  def charIndex(face : FT_Face, char : Char) : F[Int] =
    makeUnfailurable(() => FT_Get_Char_Index(face, char))
  end charIndex

  def loadGlyph(face : FT_Face, index : Int, flags : Int) : F[Unit] =
    stackPushR.use:
      stack =>
        makeF(() => (FT_Load_Glyph(face, index, flags), () => ()))
  end loadGlyph

  def setPixelSize(face : FT_Face, width : Int, height : Int) : F[Unit] =
    makeF(() =>
      (
        FT_Set_Pixel_Sizes(face, width, height),
        () => ()
      )
    )
  end setPixelSize

  def renderToContainer(face : FT_Face, renderMode : Int) : F[Unit] =
    stackPushR.use:
      stack =>
        makeF(() =>
          (
            FT_Render_Glyph(face.glyph(), renderMode),
            () => ()
          )
        )
  end renderToContainer

  override def renderChar(font : FT_Face, fontHeight : Int, char: Char): F[CharGlyph] =
    for
      index <- charIndex(font, char)
      _ <- setPixelSize(font, 0, fontHeight)
      _ <- loadGlyph(font, index, FT_LOAD_COLOR)
      _ <- renderToContainer(font, FT_RENDER_MODE_NORMAL)
      res <- charBrightness(font)
    yield res
  end renderChar

  def loadLibAndFont(path : CharSequence): Resource[F, (Long, FT_Face)] =
    for
      lib <- initLibrary
      font <- loadFont(lib, path)
    yield (lib, font)
  end loadLibAndFont

  def charBrightness(font : FT_Face) : F[CharGlyph] =
    makeUnfailurable(() =>
      val bitmap = font.glyph().bitmap()
      val buffer = bitmap.buffer(1000)
      CharGlyph(
        bitmap.rows(),
        bitmap.width(),
        (
          for
            i <- 0 until bitmap.rows
            j <- 0 until bitmap.width()
          yield buffer.get(i * bitmap.pitch() + j)
        ).toArray
      )
    )
  end charBrightness

  def brightnessToChar(value : Int) : Char =
    if value > 220 then
      '@'
    else if value > 169 then
      '*'
    else if value > 84 then
      '.'
    else
      ' '
    end if
  end brightnessToChar
end FreeTypeImpl
