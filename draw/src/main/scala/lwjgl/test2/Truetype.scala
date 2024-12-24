package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.IOUtil.ioResourceToByteBuffer
import lwjgl.test2.FontDemo.linesOfText

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW.{glfwPollEvents, glfwSwapBuffers, glfwWindowShouldClose}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.stb.STBTruetype.*
import org.lwjgl.stb.{STBTTAlignedQuad, STBTTBakedChar, STBTTFontinfo}
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryStack.stackPush

import java.nio.{ByteBuffer, IntBuffer}
import scala.util.{Try, Using}

final case class Truetype(
                            var ttf: ByteBuffer = null,
                            var info: STBTTFontinfo = null,
                            var ascent: Int = 0,
                            var descent: Int = 0,
                            var lineGap: Int = 0,
                          )

object Truetype:
  def getFontMetrics(info : STBTTFontinfo) : Try[(Int, Int, Int)] =
    Using(stackPush):
      stack =>
        val pAscent = stack.mallocInt(1)
        val pDescent = stack.mallocInt(1)
        val pLineGap = stack.mallocInt(1)
        stbtt_GetFontVMetrics(info, pAscent, pDescent, pLineGap)
        val ascent = pAscent.get(0)
        val descent = pDescent.get(0)
        val lineGap = pLineGap.get(0)
        (ascent, descent, lineGap)
  end getFontMetrics
  
  def apply() : IO[Truetype] =
    IO:
      val ttf = ioResourceToByteBuffer("JetBrainsMono-Regular.ttf", 512 * 1024)
      val info = STBTTFontinfo.create
      if (!stbtt_InitFont(info, ttf))
        throw new IllegalStateException("Failed to initialize font information.")

      val (ascent, descent, lineGap) = getFontMetrics(info).get
      Truetype(ttf, info, ascent, descent, lineGap)
  end apply

  def initLoop(self: Truetype, fontDemo : FontDemo, BITMAP_W: Int, BITMAP_H: Int): Resource[IO, STBTTBakedChar.Buffer] =
    Resource.make(
      IO:
        val texID = glGenTextures
        val cdata = STBTTBakedChar.malloc(96)
        val bitmap = BufferUtils.createByteBuffer(BITMAP_W * BITMAP_H)
        stbtt_BakeFontBitmap(self.ttf, fontDemo.line.fontHeight * fontDemo.state.contentScaleY, bitmap, BITMAP_W, BITMAP_H, 32, cdata)
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
    )(a =>
      IO:
        a.free()
    )
  end initLoop

  def loopStep(self : Truetype, fontDemo : FontDemo, cdata : STBTTBakedChar.Buffer, BITMAP_W : Int, BITMAP_H : Int) : IO[Unit] =
    IO:
      glfwPollEvents()
      glClear(GL_COLOR_BUFFER_BIT)
      val scaleFactor = 1.0f + fontDemo.state.scale * 0.25f
      glPushMatrix()
      // Zoom
      glScalef(scaleFactor, scaleFactor, 1f)
      // Scroll
      glTranslatef(4.0f, fontDemo.line.fontHeight * 0.5f + 4.0f - fontDemo.state.lineOffset * fontDemo.line.fontHeight, 0f)
      renderText(self, fontDemo, cdata, BITMAP_W, BITMAP_H)
      glPopMatrix()
      glfwSwapBuffers(fontDemo.window.window)
  end loopStep

  def shouldNotClose(window : Long) : IO[Boolean] =
    IO:
      !glfwWindowShouldClose(window)

  def loop(self : Truetype, fontDemo : FontDemo): IO[Unit] =
    val BITMAP_W = 512 * fontDemo.state.contentScaleX.round
    val BITMAP_H = 512 * fontDemo.state.contentScaleY.round
    initLoop(self, fontDemo, BITMAP_W, BITMAP_H).use(cdata =>
      loopStep(self, fontDemo, cdata, BITMAP_W, BITMAP_H).whileM_(shouldNotClose(fontDemo.window.window))
    )
  end loop

  private def scaled(center: Float, offset: Float, factor: Float) =
    (offset - center) * factor + center

  private def renderText(self : Truetype, fontDemo : FontDemo, cdata: STBTTBakedChar.Buffer, BITMAP_W: Int, BITMAP_H: Int): Unit =
    val scale = stbtt_ScaleForPixelHeight(self.info, fontDemo.line.fontHeight)
    Using(stackPush):
      stack =>
        val pCodePoint = stack.mallocInt(1)
        val x = stack.floats(0.0f)
        val y = stack.floats(0.0f)
        val q = STBTTAlignedQuad.malloc(stack)
        var lineStart = 0
        val factorX = 1.0f / fontDemo.state.contentScaleX
        val factorY = 1.0f / fontDemo.state.contentScaleY
        var lineY = 0.0f
        glBegin(GL_QUADS)
        var i = 0
        val to = fontDemo.line.text.length
        while i < to do
          i += getCP(fontDemo.line.text, to, i, pCodePoint)
          val cp = pCodePoint.get(0)
          if cp == '\n' then
            if (fontDemo.state.lineBBEnabled)
            {
              glEnd()
              renderLineBB(self, fontDemo, lineStart, i - 1, y.get(0), scale)
              glBegin(GL_QUADS)
            }
            lineY = y.get(0) + (self.ascent - self.descent + self.lineGap) * scale
            y.put(0, lineY)
            x.put(0, 0.0f)
            lineStart = i
          else if !(cp < 32) && !(128 <= cp) then
            val cpX = x.get(0)
            stbtt_GetBakedQuad(cdata, BITMAP_W, BITMAP_H, cp - 32, x, y, q, true)
            x.put(0, scaled(cpX, x.get(0), factorX))
            if fontDemo.state.kerningEnabled && i < to then
              getCP(fontDemo.line.text, to, i, pCodePoint)
              x.put(0, x.get(0) + stbtt_GetCodepointKernAdvance(self.info, cp, pCodePoint.get(0)) * scale)
            val x0 = scaled(cpX, q.x0, factorX)
            val x1 = scaled(cpX, q.x1, factorX)
            val y0 = scaled(lineY, q.y0, factorY)
            val y1 = scaled(lineY, q.y1, factorY)
            glTexCoord2f(q.s0, q.t0)
            glVertex2f(x0, y0)
            glTexCoord2f(q.s1, q.t0)
            glVertex2f(x1, y0)
            glTexCoord2f(q.s1, q.t1)
            glVertex2f(x1, y1)
            glTexCoord2f(q.s0, q.t1)
            glVertex2f(x0, y1)
          end if
        end while
        glEnd()
        if (fontDemo.state.lineBBEnabled)
          renderLineBB(self, fontDemo, lineStart, fontDemo.line.text.length, lineY, scale)
        end if
  end renderText

  private def renderLineBB(self : Truetype, fontDemo : FontDemo, from: Int, to: Int, yIn: Float, scale: Float): Unit =
    glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT, GL_LINE)
    glColor3f(1.0f, 1.0f, 0.0f)
    val width = getStringWidth(fontDemo.state.kerningEnabled, self.info, fontDemo.line.text, from, to, fontDemo.line.fontHeight)
    val y = yIn - self.descent * scale
    glBegin(GL_QUADS)
    glVertex2f(0.0f, y)
    glVertex2f(width, y)
    glVertex2f(width, y - fontDemo.line.fontHeight)
    glVertex2f(0.0f, y - fontDemo.line.fontHeight)
    glEnd()
    glEnable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT, GL_FILL)
    glColor3f(169f / 255f, 183f / 255f, 198f / 255f) // Text color
  end renderLineBB

  private def getStringWidth(isKerningEnabled : Boolean, info: STBTTFontinfo, text: String, from: Int, to: Int, fontHeight: Int) =
    var width = 0
    Using(stackPush):
      stack =>
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
    width * stbtt_ScaleForPixelHeight(info, fontHeight)
  end getStringWidth

  private def getCP(text: String, to: Int, i: Int, cpOut: IntBuffer): Int =
    val c1 = text.charAt(i)
    if Character.isHighSurrogate(c1) && i + 1 < to then
      val c2 = text.charAt(i + 1)
      if Character.isLowSurrogate(c2) then
        cpOut.put(0, Character.toCodePoint(c1, c2))
        return 2
      end if
    end if
    cpOut.put(0, c1)
    1
  end getCP

  def fontResource(state: State, line: StyledText, title: String) : Resource[IO, FontDemo] =
    Resource.make(
      initFD(state, line, title).map(FontDemo(_, line, state))
    )(fontDemo =>
      IO:
        fontDemo.destroy()
    )
  end fontResource

  def run(state: State, line: StyledText, title: String): IO[Unit] =
    for
      self <- Truetype()
      _ <- fontResource(state, line, title).use(loop(self, _))
    yield ()
  end run

  def test() : IO[Unit] =
    val text = "Hello from \n test!"
    run(
      State(0, 0, 0, false, 0, true, false),
      StyledText(text, linesOfText(text), 18),
      "Awesome title"
    )
  end test
end Truetype

object Test2 extends IOApp:
  override def run(args : List[String]) : IO[ExitCode] =
    Truetype.test().evalOn(MainThread) *> ExitCode.Success.pure[IO]