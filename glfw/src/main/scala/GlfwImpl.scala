package me.katze.gui4s.glfw

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.effect.{Async, Resource, Sync}
import cats.syntax.all.*
import me.katze.gui4s.impure.FFI
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.system.{MemoryStack, MemoryUtil}

import java.util.Objects

def stackPush[F[_] : {Sync, FFI as I}] : Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(I.delay(MemoryStack.stackPush()))
end stackPush

final case class OglWindow(id : Long) // TODO move into a class

final class GlfwImpl[F[_] : {FFI as impure, Sync}](
                                                        unsafeRunF : [A] => F[A] => A,
                                                      ) extends Glfw[F, OglWindow]:

  override def centerWindow(window: OglWindow): F[Unit] =
    windowSize(window).flatMap:
      case Size(width, height) =>
        impure.delay:
          val monitor = glfwGetPrimaryMonitor()
          val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
          glfwSetWindowPos(
            window.id,
            (vidmode.width - width) / 2,
            (vidmode.height - height) / 2
          )
  end centerWindow

  override def createOGLContext(window: OglWindow, createCapabilities : F[Unit]): F[Unit] =
    impure.delay(
      glfwMakeContextCurrent(window.id)
    ) *> createCapabilities
  end createOGLContext

  override def windowResizeCallback(window: OglWindow, callback: Size => F[Unit]): F[Unit] =
    impure.delay:
      glfwSetWindowSizeCallback(
        window.id,
        (_, width, height) =>
          unsafeRunF(callback(Size(width, height)))
      )
  end windowResizeCallback

  override def makeVisible(window: OglWindow): F[Unit] =
    impure.delay:
      glfwShowWindow(window.id)
  end makeVisible

  override def shouldClose(window: OglWindow): F[Boolean] =
    impure.delay:
      glfwWindowShouldClose(window.id)
  end shouldClose

  override def markForBeingClosed(window: OglWindow): F[Unit] =
    impure.delay:
      glfwSetWindowShouldClose(window.id, true)
  end markForBeingClosed

  override def frameBufferResizeCallback(window: OglWindow, callback: Size => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetFramebufferSizeCallback(window.id, (_, width, height) =>
        unsafeRunF(callback(Size(width, height)))
      )
      if old != null then
        old.free()
      end if
  end frameBufferResizeCallback

  override def keyCallback(window: OglWindow, callback: (Int, Int, KeyAction, KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetKeyCallback(window.id, (_, key, scancode, action, modes) =>
        unsafeRunF(callback(key, scancode, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if  
  end keyCallback

  override def swapInterval(interval: Int): F[Unit] =
    impure.delay:
      glfwSwapInterval(interval)
  end swapInterval

  override type Monitor = Long

  override def primaryMonitor : F[Long] =
    impure.delay(glfwGetPrimaryMonitor())
      .ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end primaryMonitor

  override def windowMonitor(window : OglWindow) : F[Monitor] =
    impure.delay(glfwGetWindowMonitor(window.id))
      .ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end windowMonitor

  override def createWindow(
                              title: String,
                              size : Size,
                              visible: Boolean,
                              resizeable: Boolean,
                              debugContext: Boolean
                            ): Resource[F, OglWindow] =
    Resource.make(
      for
        _ <- windowHints(visible, resizeable, debugContext)
        id <- createWindowId(size.width, size.height, title, MemoryUtil.NULL /* TODO check if monitor should be passed */)
      yield OglWindow(id)
    )(a => 
        impure.delay:
          glfwFreeCallbacks(a.id)
          glfwDestroyWindow(a.id)
    )
  end createWindow

  def createWindowId(width : Int, height : Int, title : CharSequence, monitor : Long) : F[Long] =
    impure.delay(
      glfwCreateWindow(width, height, title, monitor, MemoryUtil.NULL)
    ).ensure(
      RuntimeException("Failed to create GLFW window")
    )(_ != MemoryUtil.NULL)
  end createWindowId

  def monitorScale(monitor : Long): F[Float] =
    stackPush.use:
      s =>
        impure.delay:
          val px = s.mallocFloat(1)
          val py = s.mallocFloat(1)
          glfwGetMonitorContentScale(monitor, px, py)
          val (scaleX, scaleY) = (px.get(0), py.get(0))
          assert(scaleX == scaleY)
          scaleX
  end monitorScale

  override def primaryMonitorScale: F[Float] =
    primaryMonitor.flatMap(monitorScale)
  end primaryMonitorScale

  def windowHints(visible : Boolean, resizable : Boolean, debugContext : Boolean) : F[Unit] =
    impure.delay:
      glfwDefaultWindowHints()
      glfwWindowHint(GLFW_VISIBLE, visible.toGlfw)
      glfwWindowHint(GLFW_RESIZABLE, resizable.toGlfw)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, debugContext.toGlfw)
  end windowHints

  extension (value : Boolean)
    def toGlfw : Int =
      if value then GLFW_TRUE else GLFW_FALSE
    end toGlfw
  end extension

  override def createPrintErrorCallback: Resource[F, GLFWErrorCallback] =
    Resource.make(
      impure.delay(GLFWErrorCallback.createPrint.set())
    )(
      errorCallback =>
        impure:
          glfwSetErrorCallback(null)
          errorCallback.free()
    )
  end createPrintErrorCallback

  override def pollEvents: F[Unit] =
    impure.delay(glfwPollEvents())

  def initGlfw: F[Unit] =
    impure.delay(glfwInit()).ifM(
      ().pure[F],
      MonadError[F, Throwable].raiseError(RuntimeException("Failed to create the GLFW"))
    )
  end initGlfw
  
  def terminate : F[Unit] =
    impure.delay(glfwTerminate())
  end terminate
  
  override def windowSize(window: OglWindow): F[Size] =
    stackPush.use:
      stack =>
        impure.delay({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetWindowSize(window.id, width, height)
          Size(width.get(0), height.get(0))
        })
  end windowSize

  override def frameBufferSize(window: OglWindow): F[Size] =
    stackPush.use:
      stack =>
        impure.delay({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetFramebufferSize(window.id, width, height)
          Size(width.get(0), height.get(0))
        })
  end frameBufferSize

  override def scrollCallback(window: OglWindow, callback: (Double, Double) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetScrollCallback(window.id, (_, xoffset, yoffset) => unsafeRunF[Unit](callback(xoffset, yoffset)))
      if old != null then
        old.free()
      end if
  end scrollCallback

  override def cursorPosCallback(window: OglWindow, callback: (Double, Double) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetCursorPosCallback(window.id, (_, xpos, ypos) => unsafeRunF[Unit](callback(xpos, ypos)))
      if old != null then
        old.free()
      end if
  end cursorPosCallback

  override def mouseButtonCallback(window: OglWindow, callback: (Int, KeyAction, KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetMouseButtonCallback(window.id, (_, key, action, modes) =>
        unsafeRunF(callback(key, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end mouseButtonCallback

  override def swapBuffers(window : OglWindow): F[Unit] =
    impure.delay:
      glfwSwapBuffers(window.id)
  end swapBuffers
end GlfwImpl

object GlfwImpl:
  def apply[F[_] : {FFI, Sync}](run : [A] => F[A] => A) : Resource[F, GlfwImpl[F]] =
    Resource.make(
      {
        val res = new GlfwImpl[F](run)
        res.initGlfw.map(_ => res)
      }
    )(_.terminate)
  end apply
  
  def apply[F[_] : {FFI, Sync}](dispatcher : Dispatcher[F]) : Resource[F, GlfwImpl[F]] =
    GlfwImpl[F]([A] => (effect : F[A]) => dispatcher.unsafeRunSync(effect))
  end apply
  
  def apply[F[_] : {FFI, Async}]() : Resource[F, GlfwImpl[F]] =
    Dispatcher.sequential[F].flatMap(GlfwImpl[F](_))
  end apply
end GlfwImpl

