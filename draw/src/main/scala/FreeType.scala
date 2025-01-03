package me.katze.gui4s.draw

import cats.effect.*
import cats.syntax.all.*
import org.lwjgl.system.MemoryStack
import org.lwjgl.util.freetype.*
import org.lwjgl.util.freetype.FreeType.*

final class FreeType[F[+_] : Sync](
                                    makeF : [T] => (() => (Int, () => T)) => F[T],
                                    makeUnfailurable : [T] =>  (() => T) => F[T],
                                   ):
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

  def loadFont(lib : Long, path : String) : Resource[F, FT_Face] =
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

  def example: Resource[F, Unit] =
    for
      lib <- initLibrary
      fontR <- loadFont(lib, "") 
    yield ()  
end FreeType

