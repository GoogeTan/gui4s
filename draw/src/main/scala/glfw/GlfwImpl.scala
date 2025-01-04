package me.katze.gui4s.draw
package glfw

import impure.Impure

import cats.MonadError
import cats.effect.{MonadCancel, Resource}
import cats.syntax.all.*
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.{GL, GLUtil}
import org.lwjgl.system.{Callback, MemoryStack, MemoryUtil}

import java.util.Objects


final class GlfwImpl[F[_]](
                            unsafeRunF : [A] => F[A] => A,
                            impure: Impure[F],
                            stackPush : Resource[F, MemoryStack]
                          )(using MonadCancel[F, Throwable]) extends Glfw[F]:
  final case class OglWindow(id : Long, procs : Callback | Null)

  override type Window = OglWindow

  override def centerWindow(window: OglWindow): F[Unit] =
    windowSize(window).flatMap:
      case Size(width, height) =>
        impure.impure:
          val monitor = glfwGetPrimaryMonitor()
          val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
          glfwSetWindowPos(
            window.id,
            (vidmode.width - width) / 2,
            (vidmode.height - height) / 2
          )
  end centerWindow

  override def createOGLContext(window: OglWindow): F[Unit] =
    impure.impure:
      glfwMakeContextCurrent(window.id)
      GL.createCapabilities()
  end createOGLContext

  override def windowResizeCallback(window: OglWindow, callback: Size => F[Unit]): F[Unit] =
    impure.impure:
      glfwSetWindowSizeCallback(
        window.id,
        (_, w, h) => unsafeRunF(callback(Size(w, h)))
      )

  override def makeVisible(window: OglWindow): F[Unit] =
    impure.impure:
      glfwShowWindow(window.id)
  end makeVisible

  override def shouldClose(window: OglWindow): F[Boolean] =
    impure.impure:
      glfwWindowShouldClose(window.id)
  end shouldClose

  override def markForBeingClosed(window: OglWindow): F[Unit] =
    impure.impure:
      glfwSetWindowShouldClose(window.id, true)
  end markForBeingClosed

  override def frameBufferResizeCallback(window: OglWindow, callback: Size => F[Unit]): F[Unit] =
    impure.impure:
      glfwSetFramebufferSizeCallback(window.id, (_, w, h) =>
        unsafeRunF(callback(Size(w, h)))
      )
  end frameBufferResizeCallback

  override def keyCallback(window: OglWindow, callback: (Int, Int, KeyAction, KeyModes) => F[Unit]): F[Unit] =
    impure.impure:
      glfwSetKeyCallback(window.id, (_, key, scancode, action, modes) =>
        unsafeRunF(callback(key, scancode, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
  end keyCallback

  override def swapInterval(interval: Int): F[Unit] =
    impure.impure:
      glfwSwapInterval(interval)
  end swapInterval

  def currentMonitor : F[Long] =
    impure.impure(glfwGetPrimaryMonitor()).ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)

  def debugMessageCallback : F[Callback | Null] =
    impure.impure(GLUtil.setupDebugMessageCallback())
  end debugMessageCallback
  
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
        monitor <- currentMonitor
        (scaleX, scaleY) <- monitorScale(monitor)
        id <- createWindowId(size.width * scaleX.round, size.height * scaleY.round, title, MemoryUtil.NULL /* TODO check if monitor should be passed */)
        callback <- debugMessageCallback
      yield OglWindow(id, callback)
    )(a => 
        impure.impure:
          if a.procs != null then 
            a.procs.free()
          end if

          GL.setCapabilities(null)
          glfwFreeCallbacks(a.id)
          glfwDestroyWindow(a.id)
          glfwTerminate()
          Objects.requireNonNull(glfwSetErrorCallback(null)).free()
    )
  end createWindow

  def createWindowId(width : Int, height : Int, title : CharSequence, monitor : Long) : F[Long] =
    impure.impure(
      glfwCreateWindow(width, height, title, monitor, MemoryUtil.NULL)
    ).ensure(
      RuntimeException("Failed to create GLFW window")
    )(_ != MemoryUtil.NULL)
  end createWindowId

  def monitorScale(monitor : Long): F[(Float, Float)] =
    stackPush.use:
      s =>
        impure.impure:
          val px = s.mallocFloat(1)
          val py = s.mallocFloat(1)
          glfwGetMonitorContentScale(monitor, px, py)
          (px.get(0), py.get(0))
  end monitorScale

  def windowHints(visible : Boolean, resizable : Boolean, debugContext : Boolean) : F[Unit] =
    impure.impure:
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

  override def createPrintErrorCallback: F[Unit] =
    impure.impure(GLFWErrorCallback.createPrint.set())
  end createPrintErrorCallback

  override def setupDebugMessageCallback: F[Unit] =
    impure.impure(GLUtil.setupDebugMessageCallback())

  override def pollEvents: F[Unit] =
    impure.impure(glfwPollEvents())

  override def initGlfw: F[Unit] =
    impure.impure(glfwInit()).ifM(
      ().pure[F],
      MonadError[F, Throwable].raiseError(RuntimeException("Failed to create the GLFW window"))
    )
  end initGlfw

  override def windowSize(window: OglWindow): F[Size] =
    stackPush.use:
      stack =>
        impure.impure({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetWindowSize(window.id, width, height)
          Size(width.get(0), height.get(0))
        })
  end windowSize

  override def frameBufferSize(window: OglWindow): F[Size] =
    stackPush.use:
      stack =>
        impure.impure({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetFramebufferSize(window.id, width, height)
          Size(width.get(0), height.get(0))
        })
  end frameBufferSize
end GlfwImpl

