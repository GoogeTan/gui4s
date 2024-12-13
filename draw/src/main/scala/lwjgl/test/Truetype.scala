package me.katze.gui4s.draw
package lwjgl.test

import cats.effect.std.Dispatcher
import cats.effect.{IO, Resource}
import org.lwjgl.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.stb.*
import org.lwjgl.stb.STBTruetype.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*

import java.nio.*
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.ExecutionContext


@SuppressWarnings(Array("org.wartremover.warts.All"))
def getCP(text: String, to: Int, i: Int, cpOut: IntBuffer): Int =
  val c1 = text.charAt(i)
  if (Character.isHighSurrogate(c1) && i + 1 < to) then
    val c2 = text.charAt(i + 1)
    if Character.isLowSurrogate(c2) then
      cpOut.put(0, Character.toCodePoint(c1, c2))
      return 2
    end if
  end if
  cpOut.put(0, c1)
  1
end getCP

@SuppressWarnings(Array("org.wartremover.warts.All"))
def readAscentDescentAndLineGap(info : STBTTFontinfo) : (Int, Int, Int) =
  val stack: MemoryStack = stackPush
  var ascent : Int = 0
  var descent : Int = 0
  var lineGap : Int = 0
  try
    val pAscent = stack.mallocInt(1)
    val pDescent = stack.mallocInt(1)
    val pLineGap = stack.mallocInt(1)
    stbtt_GetFontVMetrics(info, pAscent, pDescent, pLineGap)
    ascent = pAscent.get(0)
    descent = pDescent.get(0)
    lineGap = pLineGap.get(0)
  finally
    if stack != null then
      stack.close()
    end if
  end try
  (ascent, descent, lineGap)

def initBakedCharBuffer(font : Font, window : GlfwWindow, pixelHeight : Float) : Resource[IO, STBTTBakedChar.Buffer] =
  val BITMAP_W = 512 * window.contentScaleY.round
  val BITMAP_H = 512 * window.contentScaleY.round
  Resource.make(
    IO:
      val texID = glGenTextures
      val cdata = STBTTBakedChar.malloc(96)
      val bitmap = BufferUtils.createByteBuffer(BITMAP_W * BITMAP_H)
      stbtt_BakeFontBitmap(font.ttf, pixelHeight, bitmap, BITMAP_W, BITMAP_H, 32, cdata)
      glBindTexture(GL_TEXTURE_2D, texID)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, BITMAP_W, BITMAP_H, 0, GL_ALPHA, GL_UNSIGNED_BYTE, bitmap)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glClearColor(43f / 255f, 43f / 255f, 43f / 255f, 0f) // BG color

      glColor3f(169f / 255f, 183f / 255f, 198f / 255f) // Text color

      glEnable(GL_TEXTURE_2D)
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      cdata
  )(
    a => IO.apply(a.free())
  )
end initBakedCharBuffer

def shouldNotWindowClose(window: GlfwWindow) : IO[Boolean] =
  IO:
    !glfwWindowShouldClose(window.openGlWindow)
end shouldNotWindowClose

@SuppressWarnings(Array("org.wartremover.warts.All"))
def renderText(
                window : GlfwWindow,
                cdata: STBTTBakedChar.Buffer,
                text : String,
                font : Font,
                fontHeight : Int,
                isLineBBEnabled : Boolean,
                isKerningEnabled : Boolean,
              ) : IO[Unit] =
  val BITMAP_W = 512 * window.contentScaleY.round
  val BITMAP_H = 512 * window.contentScaleY.round
  def getStringWidth(info: STBTTFontinfo, text: String, from: Int, to: Int, fontHeight: Int) =
    var width = 0
    val stack = stackPush
    try
      val pCodePoint = stack.mallocInt(1)
      val pAdvancedWidth = stack.mallocInt(1)
      val pLeftSideBearing = stack.mallocInt(1)
      var i = from
      while i < to do
        i += getCP(text, to, i, pCodePoint)
        val cp = pCodePoint.get(0)
        stbtt_GetCodepointHMetrics(info, cp, pAdvancedWidth, pLeftSideBearing)
        width += pAdvancedWidth.get(0)
        if isKerningEnabled && i < to then
          getCP(text, to, i, pCodePoint)
          width += stbtt_GetCodepointKernAdvance(info, cp, pCodePoint.get(0))
        end if
      end while
    finally
      if stack != null then
        stack.close()
      end if
    end try
    width * stbtt_ScaleForPixelHeight(info, fontHeight)
  end getStringWidth

  def renderLineBB(from: Int, to: Int, yIn: Float, scale: Float): Unit =
    var y = yIn
    glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT, GL_LINE)
    glColor3f(1.0f, 1.0f, 0.0f)
    val width = getStringWidth(font.info, text, from, to, fontHeight)
    y -= font.descent * scale
    glBegin(GL_QUADS)
    glVertex2f(0.0f, y)
    glVertex2f(width, y)
    glVertex2f(width, y - fontHeight)
    glVertex2f(0.0f, y - fontHeight)
    glEnd()
    glEnable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT, GL_FILL)
    glColor3f(169f / 255f, 183f / 255f, 198f / 255f) // Text color
  end renderLineBB

  IO:
    val scale = stbtt_ScaleForPixelHeight(font.info, fontHeight)
    val stack = stackPush
    try
      val pCodePoint = stack.mallocInt(1)
      val x = stack.floats(0.0f)
      val y = stack.floats(0.0f)
      val quad = STBTTAlignedQuad.malloc(stack)
      var lineStart = 0
      val factorX = 1.0f / window.contentScaleX
      val factorY = 1.0f / window.contentScaleY
      var lineY = 0.0f
      glBegin(GL_QUADS)
      var i = 0
      val to = text.length
      while i < to do
        i += getCP(text, to, i, pCodePoint)
        val cp = pCodePoint.get(0)
        if cp == '\n' then
          if isLineBBEnabled then
            glEnd()
            renderLineBB(lineStart, i - 1, y.get(0), scale)
            glBegin(GL_QUADS)
          end if
          lineY = y.get(0) + (font.ascent - font.descent + font.lineGap) * scale
          y.put(0, lineY)
          x.put(0, 0.0f)
          lineStart = i
        else if cp >= 32 && 128 > cp then
          val cpX = x.get(0)
          stbtt_GetBakedQuad(cdata, BITMAP_W, BITMAP_H, cp - 32, x, y, quad, true)
          x.put(0, Truetype.scaled(cpX, x.get(0), factorX))
          if isKerningEnabled && i < to then
            getCP(text, to, i, pCodePoint)
            x.put(0, x.get(0) + stbtt_GetCodepointKernAdvance(font.info, cp, pCodePoint.get(0)) * scale)
          val x0 = Truetype.scaled(cpX, quad.x0, factorX)
          val x1 = Truetype.scaled(cpX, quad.x1, factorX)
          val y0 = Truetype.scaled(lineY, quad.y0, factorY)
          val y1 = Truetype.scaled(lineY, quad.y1, factorY)
          glTexCoord2f(quad.s0, quad.t0)
          glVertex2f(x0, y0)
          glTexCoord2f(quad.s1, quad.t0)
          glVertex2f(x1, y0)
          glTexCoord2f(quad.s1, quad.t1)
          glVertex2f(x1, y1)
          glTexCoord2f(quad.s0, quad.t1)
          glVertex2f(x0, y1)
        end if
      end while
      glEnd()
      if isLineBBEnabled then
        renderLineBB(lineStart, text.length, lineY, scale)
      end if
    finally
      if stack != null then
        stack.close()
      end if
    end try
end renderText

def drawStage(windowGetter: IO[GlfwWindow], draw : IO[Unit]) : IO[Unit] =
  IO:
    glfwPollEvents()
    glClear(GL_COLOR_BUFFER_BIT)
  *> draw
  *>
    windowGetter flatMap (
      window =>
        IO:
          glfwSwapBuffers(window.openGlWindow)
    )
end drawStage

def drawLoop(window: IO[GlfwWindow], draw : IO[Unit]) : IO[Unit] =
  drawStage(window, draw).whileM_(window.flatMap(shouldNotWindowClose))
end drawLoop

@SuppressWarnings(Array("org.wartremover.warts.All"))
object Truetype:

  def scaled(center: Float, offset: Float, factor: Float) =
    (offset - center) * factor + center
  end scaled
end Truetype