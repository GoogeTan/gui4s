package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.GLFWUtil.glfwInvoke

import cats.effect.{IO, Resource}
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

final case class OGLWindow(
                            window: Long,
                            var ww: Int,
                            var wh: Int,
                            debugProc: Callback | Null,
                          ):
  def destroy(): Unit =
    GL.setCapabilities(null)
    if (debugProc != null)
      debugProc.free()
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end destroy
end OGLWindow

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


def lineHeight(styledText: StyledText, state: State): Float =
  styledText.fontHeight * (1.0f + state.scale * 0.25f)
end lineHeight

def setLineOffset(offset: Float, state: State, line: StyledText, window: OGLWindow): Unit =
  setLineOffset(offset.round, state, line, window)
end setLineOffset

def setLineOffset(offset: Int, state : State, line : StyledText, window : OGLWindow): Unit =
  state.lineOffset = max(0, min(offset, line.lineCount - (window.wh / lineHeight(line, state)).toInt))
end setLineOffset

def setScale(scale: Int, state : State, line : StyledText, window : OGLWindow): Unit =
  state.scale = max(-3, scale)
  //line.lineHeight = line.fontHeight * (1.0f + state.scale * 0.25f)
  setLineOffset(state.lineOffset, state, line, window)

final case class FontDemo(
                            window : OGLWindow,
                            line : StyledText,
                            state : State
                          ):
  def destroy(): Unit =
    window.destroy()
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
end FontDemo

def stackPushResource : Resource[IO, MemoryStack] =
  Resource.fromAutoCloseable(
    IO:
      stackPush
  )
end stackPushResource

def initFrameBuffer(monitor : Long, self : State, ww : Int, wh : Int) : IO[(Int, Int)] =
  stackPushResource.use:
    s =>
      IO:
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

def errorCallbackToPrint : IO[Unit] =
  IO:
    GLFWErrorCallback.createPrint.set

def ensureGlfwInit : IO[Unit] =
  (
    IO:
      glfwInit()
  ).ifM(
    IO.unit,
    IO.raiseError(new IllegalStateException("Unable to initialize GLFW"))
  )

def defaultWindowHints : IO[Unit] =
  IO:
    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)

def initMonitor : IO[Long] =
  IO:
    glfwGetPrimaryMonitor

def initWindow(state : State, title : String) : IO[Long] =
  for
    monitor <- initMonitor
    (framebufferW, framebufferH) <- initFrameBuffer(monitor, state, 800, 600)
    res <- IO:
      val window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
      if window == NULL then
        throw new RuntimeException("Failed to create the GLFW window")
      // Center window
      val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
      glfwSetWindowPos(window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
      window
  yield res
end initWindow

def initWindow(state: State, line: StyledText, title: String): IO[OGLWindow] =
  for
    _ <- errorCallbackToPrint *> ensureGlfwInit *> defaultWindowHints
    window <- initWindow(state, title)
    res <-
      IO:
        // Create context
        glfwMakeContextCurrent(window)
        GL.createCapabilities
        val debugProc = GLUtil.setupDebugMessageCallback
        glfwSwapInterval(1)
        glfwShowWindow(window)
        val res = OGLWindow(window, 800, 600, debugProc)
        glfwInvoke(window, windowSizeChanged2(state, res, line, _, _, _), framebufferSizeChanged)
        res
    _ <- registerCallbacks(state, res, line)
  yield res
end initWindow

def registerCallbacks(state: State, oglWindow: OGLWindow, line: StyledText) : IO[Unit] =
  IO:
    glfwSetWindowSizeCallback(oglWindow.window, windowSizeChanged2(state, oglWindow, line, _, _, _))
    glfwSetFramebufferSizeCallback(oglWindow.window, framebufferSizeChanged)
    glfwSetKeyCallback(oglWindow.window, (window: Long, key: Int, _: Int, action: Int, _: Int) =>
      key match
        case GLFW_KEY_LEFT_CONTROL =>
        case GLFW_KEY_RIGHT_CONTROL =>
          state.ctrlDown = action != GLFW_RELEASE
        case _ => ()
      end match

      if action != GLFW_RELEASE then
        key match
          case GLFW_KEY_ESCAPE =>
            glfwSetWindowShouldClose(window, true)
          case GLFW_KEY_PAGE_UP =>
            setLineOffset(state.lineOffset - oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_PAGE_DOWN =>
            setLineOffset(state.lineOffset + oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_HOME =>
            setLineOffset(0, state, line, oglWindow)
          case GLFW_KEY_END =>
            setLineOffset(line.lineCount - oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_KP_ADD =>
          case GLFW_KEY_EQUAL =>
            setScale(state.scale + 1, state, line, oglWindow)
          case GLFW_KEY_KP_SUBTRACT =>
          case GLFW_KEY_MINUS =>
            setScale(state.scale - 1, state, line, oglWindow)
          case GLFW_KEY_0 =>
          case GLFW_KEY_KP_0 =>
            if (state.ctrlDown)
              setScale(0, state, line, oglWindow)
          case GLFW_KEY_B =>
            state.lineBBEnabled = !state.lineBBEnabled

          case GLFW_KEY_K =>
            state.kerningEnabled = !state.kerningEnabled
          case _ => ()
      end if
    )
    glfwSetScrollCallback(oglWindow.window, (_: Long, _: Double, yoffset: Double) =>
      if (state.ctrlDown)
        setScale(state.scale + yoffset.round.toInt, state, line, oglWindow)
      else
        setLineOffset(state.lineOffset - yoffset.round.toInt * 3, state, line, oglWindow)
    )
end registerCallbacks

def framebufferSizeChanged(window: Long, width: Int, height: Int): Unit =
  glViewport(0, 0, width, height)
end framebufferSizeChanged

def windowSizeChanged2(state: State, oglWindow: OGLWindow, line: StyledText, window: Long, widthIn: Int, heightIn: Int): Unit =
  var width = widthIn
  var height = heightIn
  if Platform.get ne Platform.MACOSX then
    width = (width / state.contentScaleX).toInt
    height = (height / state.contentScaleY).toInt
  end if

  oglWindow.ww = width
  oglWindow.wh = height
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
  glMatrixMode(GL_MODELVIEW)
  setLineOffset(state.lineOffset, state, line, oglWindow)
end windowSizeChanged2