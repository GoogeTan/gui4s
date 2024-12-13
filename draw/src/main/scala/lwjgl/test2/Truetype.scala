package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.IOUtil.ioResourceToByteBuffer

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
                            fontDemo: FontDemo,
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


  def apply(text : String) : Truetype =
    val font = FontDemo(text, 24)
    val ttf = ioResourceToByteBuffer("JetBrainsMono-Regular.ttf", 512 * 1024)
    val info = STBTTFontinfo.create
    if (!stbtt_InitFont(info, ttf))
      throw new IllegalStateException("Failed to initialize font information.")

    val (ascent, descent, lineGap) = getFontMetrics(info).get
    Truetype(font, ttf, info, ascent, descent, lineGap)
  end apply

  def init(self : Truetype, BITMAP_W: Int, BITMAP_H: Int): STBTTBakedChar.Buffer =
    val texID = glGenTextures
    val cdata = STBTTBakedChar.malloc(96)
    val bitmap = BufferUtils.createByteBuffer(BITMAP_W * BITMAP_H)
    stbtt_BakeFontBitmap(self.ttf, self.fontDemo.fontHeight * self.fontDemo.contentScaleY, bitmap, BITMAP_W, BITMAP_H, 32, cdata)
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
  end init

  def loop(self : Truetype): Unit =
    val BITMAP_W = 512 * self.fontDemo.contentScaleX.round
    val BITMAP_H = 512 * self.fontDemo.contentScaleY.round
    val cdata = init(self, BITMAP_W, BITMAP_H)
    while (!glfwWindowShouldClose(self.fontDemo.window)) do
      glfwPollEvents()
      glClear(GL_COLOR_BUFFER_BIT)
      val scaleFactor = 1.0f + self.fontDemo.scale * 0.25f
      glPushMatrix()
      // Zoom
      glScalef(scaleFactor, scaleFactor, 1f)
      // Scroll
      glTranslatef(4.0f, self.fontDemo.fontHeight * 0.5f + 4.0f - self.fontDemo.lineOffset * self.fontDemo.fontHeight, 0f)
      renderText(self, cdata, BITMAP_W, BITMAP_H)
      glPopMatrix()
      glfwSwapBuffers(self.fontDemo.window)
    end while
    cdata.free()
  end loop

  private def scaled(center: Float, offset: Float, factor: Float) =
    (offset - center) * factor + center

  private def renderText(self : Truetype, cdata: STBTTBakedChar.Buffer, BITMAP_W: Int, BITMAP_H: Int): Unit =
    val scale = stbtt_ScaleForPixelHeight(self.info, self.fontDemo.fontHeight)
    Using(stackPush):
      stack =>
        val pCodePoint = stack.mallocInt(1)
        val x = stack.floats(0.0f)
        val y = stack.floats(0.0f)
        val q = STBTTAlignedQuad.malloc(stack)
        var lineStart = 0
        val factorX = 1.0f / self.fontDemo.contentScaleX
        val factorY = 1.0f / self.fontDemo.contentScaleY
        var lineY = 0.0f
        glBegin(GL_QUADS)
        var i = 0
        val to = self.fontDemo.text.length
        while (i < to) do
          i += getCP(self.fontDemo.text, to, i, pCodePoint)
          val cp = pCodePoint.get(0)
          if cp == '\n' then
            if (self.fontDemo.lineBBEnabled)
            {
              glEnd()
              renderLineBB(self, lineStart, i - 1, y.get(0), scale)
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
            if self.fontDemo.kerningEnabled && i < to then
              getCP(self.fontDemo.text, to, i, pCodePoint)
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
        if (self.fontDemo.lineBBEnabled)
          renderLineBB(self, lineStart, self.fontDemo.text.length, lineY, scale)
        end if
  end renderText

  private def renderLineBB(self : Truetype, from: Int, to: Int, yIn: Float, scale: Float): Unit =

    glDisable(GL_TEXTURE_2D)
    glPolygonMode(GL_FRONT, GL_LINE)
    glColor3f(1.0f, 1.0f, 0.0f)
    val width = getStringWidth(self.fontDemo.kerningEnabled, self.info, self.fontDemo.text, from, to, self.fontDemo.fontHeight)
    val y = yIn - self.descent * scale
    glBegin(GL_QUADS)
    glVertex2f(0.0f, y)
    glVertex2f(width, y)
    glVertex2f(width, y - self.fontDemo.fontHeight)
    glVertex2f(0.0f, y - self.fontDemo.fontHeight)
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
        while (i < to) do
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

  def run(self : Truetype, title: String): Unit =
    try
      initFD(self.fontDemo, title)
      loop(self)
    finally
      try
        self.fontDemo.destroy()
      catch
        case e: Exception =>
          e.printStackTrace()
      end try
    end try
  end run

  @main
  def test() : Unit =
    run(Truetype("Hello from test!"), "Awesome title")
  end test
end Truetype
