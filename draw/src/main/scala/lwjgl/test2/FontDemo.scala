package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.GLFWUtil.glfwInvoke

import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.{GL, GLUtil}
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.{Callback, MemoryStack, Platform}

import java.lang.Math.{max, min}
import java.util.Objects
import java.util.regex.Pattern
import scala.util.{Try, Using}

final case class OGLWindow(
                            window: Long,
                            var ww: Int,
                            var wh: Int,
                            debugProc: Callback | Null,
                          )

final case class StyledText(
                            text : String,
                            lineCount: Int,
                            fontHeight: Int,
                          )

final case class State(
                        var scale: Int,
                        var contentScaleX: Float,
                        var contentScaleY: Float,
                        var ctrlDown: Boolean,
                        var lineOffset: Int,
                        var kerningEnabled: Boolean,
                        var lineBBEnabled: Boolean,
                      )


final case class FontDemo(
                            window : OGLWindow,
                            line : StyledText,
                            state : State
                          ):
  def lineHeight: Float = line.fontHeight * (1.0f + state.scale * 0.25f)
  
  def setLineOffset(offset: Float): Unit =
    setLineOffset(offset.round)

  def setLineOffset(offset: Int): Unit =
    state.lineOffset = max(0, min(offset, line.lineCount - (window.wh / lineHeight).toInt))

  def setScale(scale: Int): Unit =
    state.scale = max(-3, scale)
    //line.lineHeight = line.fontHeight * (1.0f + state.scale * 0.25f)
    setLineOffset(state.lineOffset)

  def destroy(): Unit =
    GL.setCapabilities(null)
    if (window.debugProc != null)
      window.debugProc.free()
    glfwFreeCallbacks(window.window)
    glfwDestroyWindow(window.window)
    glfwTerminate()
    Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end destroy
end FontDemo

object FontDemo:
  def linesOfText(text : String) : Int =
    var lc = 0
    val m = Pattern.compile("^.*$", Pattern.MULTILINE).matcher(text)
    while m.find do
      lc += 1
    end while
    lc
  end linesOfText

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  def apply(text : String, fontHeight : Int) : FontDemo =
    new FontDemo(
      OGLWindow(0, 800, 600, null),
      StyledText(text, linesOfText(text), fontHeight),
      State(0, 0, 0, false, 0, true, false)
    )
  end apply
end FontDemo

def initFrameBuffer(monitor : Long, self : State, ww : Int, wh : Int) : Try[(Int, Int)] =
  Using(stackPush):
    s =>
      val px = s.mallocFloat(1)
      val py = s.mallocFloat(1)
      glfwGetMonitorContentScale(monitor, px, py)
      self.contentScaleX = px.get(0)
      self.contentScaleY = py.get(0)
      if Platform.get eq Platform.MACOSX then
        (ww, wh)
      else
        (ww * self.contentScaleX.round, wh * self.contentScaleY.round)
end initFrameBuffer


def initFD(state: State, line: StyledText, title: String): OGLWindow =
  GLFWErrorCallback.createPrint.set
  if (!glfwInit)
    throw new IllegalStateException("Unable to initialize GLFW")
  glfwDefaultWindowHints()
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)
  val monitor = glfwGetPrimaryMonitor
  val (framebufferW, framebufferH) = initFrameBuffer(monitor, state, 800, 600).get
  val window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
  if (window == NULL)
    throw new RuntimeException("Failed to create the GLFW window")
  // Center window
  val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
  glfwSetWindowPos(window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
  // Create context
  glfwMakeContextCurrent(window)
  GL.createCapabilities
  val debugProc = GLUtil.setupDebugMessageCallback
  glfwSwapInterval(1)
  glfwShowWindow(window)
  val res = OGLWindow(window, 800, 600, debugProc)
  val self = FontDemo(res, line, state)
  glfwInvoke(window, windowSizeChanged2(self, _, _, _), framebufferSizeChanged)
  registerCallbacks(self)
  
  res
end initFD

def registerCallbacks(self : FontDemo) : Unit =
  glfwSetWindowSizeCallback(self.window.window, windowSizeChanged2(self, _, _, _))
  glfwSetFramebufferSizeCallback(self.window.window, framebufferSizeChanged)
  glfwSetKeyCallback(self.window.window, (window: Long, key: Int, _: Int, action: Int, _: Int) =>
    key match
      case GLFW_KEY_LEFT_CONTROL =>
      case GLFW_KEY_RIGHT_CONTROL =>
        self.state.ctrlDown = action != GLFW_RELEASE
      case _ => ()  
    end match

    if action != GLFW_RELEASE then
      key match
        case GLFW_KEY_ESCAPE =>
          glfwSetWindowShouldClose(window, true)
        case GLFW_KEY_PAGE_UP =>
          self.setLineOffset(self.state.lineOffset - self.window.wh / self.lineHeight)
        case GLFW_KEY_PAGE_DOWN =>
          self.setLineOffset(self.state.lineOffset + self.window.wh / self.lineHeight)
        case GLFW_KEY_HOME =>
          self.setLineOffset(0)
        case GLFW_KEY_END =>
          self.setLineOffset(self.line.lineCount - self.window.wh / self.lineHeight)
        case GLFW_KEY_KP_ADD =>
        case GLFW_KEY_EQUAL =>
          self.setScale(self.state.scale + 1)
        case GLFW_KEY_KP_SUBTRACT =>
        case GLFW_KEY_MINUS =>
          self.setScale(self.state.scale - 1)
        case GLFW_KEY_0 =>
        case GLFW_KEY_KP_0 =>
          if (self.state.ctrlDown)
            self.setScale(0)
        case GLFW_KEY_B =>
          self.state.lineBBEnabled = !self.state.lineBBEnabled

        case GLFW_KEY_K =>
          self.state.kerningEnabled = !self.state.kerningEnabled
        case _ => ()  
    end if
  )
  glfwSetScrollCallback(self.window.window, (_: Long, _: Double, yoffset: Double) =>
    if (self.state.ctrlDown)
      self.setScale(self.state.scale + yoffset.round.toInt)
    else
      self.setLineOffset(self.state.lineOffset - yoffset.round.toInt * 3)
  )
end registerCallbacks

def framebufferSizeChanged(window: Long, width: Int, height: Int): Unit =
  glViewport(0, 0, width, height)
end framebufferSizeChanged

def windowSizeChanged2(self : FontDemo, window: Long, widthIn: Int, heightIn: Int): Unit =
  var width = widthIn
  var height = heightIn
  if Platform.get ne Platform.MACOSX then
    width = (width / self.state.contentScaleX).toInt
    height = (height / self.state.contentScaleY).toInt
  end if

  self.window.ww = width
  self.window.wh = height
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
  glMatrixMode(GL_MODELVIEW)
  self.setLineOffset(self.state.lineOffset)
end windowSizeChanged2