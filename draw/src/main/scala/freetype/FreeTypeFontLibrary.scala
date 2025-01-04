package me.katze.gui4s.draw
package freetype

import impure.ImpureError

import cats.*
import cats.effect.*
import cats.syntax.all.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.util.freetype.*
import org.lwjgl.util.freetype.FreeType.*

import java.nio.ShortBuffer

final class FreeTypeFontLibrary[F[_] : Sync](impure : ImpureError[F, Int]) extends FontLibrary[F]:
  override type Font = FT_Face
  override type Library = Long

  def stackPushR : Resource[F, MemoryStack] =
    Resource.fromAutoCloseable(impure.impureTry(Right(MemoryStack.stackPush())))
  end stackPushR
  
  def initLibrary : Resource[F, Long] =
    Resource.make(
      stackPushR.use:
        stack =>
          impure.impureTry:
            val pp = stack.mallocPointer(1)
            val error = FT_Init_FreeType(pp)
            if error != 0 then
              Left(error)
            else
              Right(pp.get(0))
            end if
    )(lib =>
      impure.impureTry:
        val error = FT_Done_FreeType(lib)
        if error != 0 then
          Left(error)
        else
          Right(())  
        end if
    )
  end initLibrary

  def libraryVersion(lib : Long) : F[(Int, Int, Int)] =
    stackPushR.use:
      stack =>
        impure.impureTry:
          val major = stack.mallocInt(1)
          val minor = stack.mallocInt(1)
          val patch = stack.mallocInt(1)
          FT_Library_Version(lib, major, minor, patch)
          Right((major.get(0), minor.get(0), patch.get(0)))
  end libraryVersion

  def loadFont(lib : Long, path : CharSequence): Resource[F, FT_Face] =
    Resource.make(
      stackPushR.use:
        stack =>
          impure.impureTry:
            val face = stack.mallocPointer(1)
            val error = FT_New_Face(lib, path, 0, face)
            if error != 0 then 
              Left(error)
            else
              Right(FT_Face.createSafe(face.get(0)))
    )(face =>
      impure.impureTry:
        val error = FT_Done_Face(face)
        if error != 0 then
          Left(error)
        else
          Right(())
        end if
    )
  end loadFont

  def charIndex(face : FT_Face, char : Char) : F[Int] =
    impure.impureTry:
      Right(FT_Get_Char_Index(face, char))
  end charIndex

  def loadGlyph(face : FT_Face, index : Int, flags : Int) : F[Unit] =
    stackPushR.use:
      stack =>
        impure.impureTry:
          val error = FT_Load_Glyph(face, index, flags)
          if error != 0 then
            Left(error)
          else
            Right(())
          end if
  end loadGlyph

  def setPixelSize(face : FT_Face, width : Int, height : Int) : F[Unit] =
    impure.impureTry:
      val error = FT_Set_Pixel_Sizes(face, width, height) 
      if error != 0 then
        Left(error)
      else
        Right(())
      end if
  end setPixelSize

  def renderToContainer(face : FT_Face, renderMode : Int) : F[Unit] =
    stackPushR.use:
      stack =>
        impure.impureTry:
          val error = FT_Render_Glyph(face.glyph(), renderMode)
          if error != 0 then
            Left(error)
          else
            Right(())
          end if
  end renderToContainer

  override def rasterChar(font : FT_Face, fontHeight : Int, char: Char): F[CharGlyph] =
    for
      index <- charIndex(font, char)
      _ <- setPixelSize(font, 0, fontHeight)
      _ <- loadGlyph(font, index, FT_LOAD_COLOR)
      _ <- renderToContainer(font, FT_RENDER_MODE_NORMAL)
      res <- charBrightness(font)
    yield res
  end rasterChar

  def loadLibAndFont(path : CharSequence): Resource[F, (Long, FT_Face)] =
    for
      lib <- initLibrary
      font <- loadFont(lib, path)
    yield (lib, font)
  end loadLibAndFont

  def charBrightness(font : FT_Face) : F[CharGlyph] =
    impure.impureTry:
      val bitmap = font.glyph().bitmap()
      val buffer = bitmap.buffer(1000)
      Right(
        CharGlyph(
          bitmap.rows(),
          bitmap.width(),
          ShortBuffer.wrap(
            (
              for
                i <- 0 until bitmap.rows
                j <- 0 until bitmap.width()
              yield buffer.get(i * bitmap.pitch() + j).toShort
            ).toArray
          )
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
end FreeTypeFontLibrary
