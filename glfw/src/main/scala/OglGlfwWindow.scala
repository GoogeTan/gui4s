package gui4s.glfw

import catnip.ForeignFunctionInterface
import cats.effect.Sync
import cats.syntax.all.*
import gui4s.core.geometry.{Point2d, Rect}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.MemoryUtil

final case class OglGlfwWindow(id: Long)
  
final class OglWindowIsGlfwWindow[F[_] : Sync](
                                                impure : ForeignFunctionInterface[F],
                                                unsafeRunF: F[Unit] => Unit
                                              ) extends GlfwWindow[F, OglGlfwWindow, Long, Float]:
  override def center(window: OglGlfwWindow): F[Unit] =
    size(window).flatMap:
      case Rect(width, height) =>
        impure.delay:
          val monitor = glfwGetPrimaryMonitor()
          val vidmode = glfwGetVideoMode(monitor) // TODO raise error on null
          glfwSetWindowPos(
            window.id,
            (vidmode.width - width.toInt) / 2,
                  (vidmode.height - height.toInt) / 2
                )
  end center

  override def makeVisible(window: OglGlfwWindow): F[Unit] =
    impure.delay:
      glfwShowWindow(window.id)
  end makeVisible

  override def shouldClose(window: OglGlfwWindow): F[Boolean] =
    impure.delay:
      glfwWindowShouldClose(window.id)
  end shouldClose

  override def markForBeingClosed(window: OglGlfwWindow): F[Unit] =
    impure.delay:
      glfwSetWindowShouldClose(window.id, true)
  end markForBeingClosed

  def getScale(window: OglGlfwWindow): F[(Float, Float)] =
    stackPush(impure).use:
      stack =>
        impure.delay:
          val width = stack.mallocFloat(1)
          val height = stack.mallocFloat(1)
          glfwGetWindowContentScale(window.id, width, height)
          (width.get(0), height.get(0))
  end getScale

  override def frameBufferResizeCallback(window: OglGlfwWindow)(callback: Rect[Float] => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetFramebufferSizeCallback(window.id, (_, width, height) =>
        unsafeRunF(callback(Rect(width, height)))
      )
      if old != null then
        old.free()
      end if
  end frameBufferResizeCallback

  override def keyCallback(window: OglGlfwWindow)(callback: (key: Int, scanCode: Int, keyAction: KeyAction, keyModes: KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetKeyCallback(window.id, (_, key, scancode, action, modes) =>
        unsafeRunF(callback(key, scancode, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end keyCallback

  override def monitor(window: OglGlfwWindow): F[Long] = // TODO a typed error
    impure.delay(glfwGetWindowMonitor(window.id)).ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end monitor

  override def size(window: OglGlfwWindow): F[Rect[Float]] =
    getScale(window).flatMap:
      (xScale, yScale) =>
        stackPush(impure).use:
          stack =>
            impure.delay({
              val width = stack.mallocInt(1)
              val height = stack.mallocInt(1)
              glfwGetWindowSize(window.id, width, height)
              Rect(width.get(0) * xScale, height.get(0) * yScale)
            })
  end size

  override def frameBufferSize(window: OglGlfwWindow): F[Rect[Float]] =
    stackPush(impure).use:
      stack =>
        impure.delay:
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetFramebufferSize(window.id, width, height)
          Rect(width.get(0), height.get(0))
  end frameBufferSize

  //TODO проверить, правда ли это скрин координаты, а не пиксели.
  override def scrollCallback(window: OglGlfwWindow)(callback: (xoffset: Float, yoffset: Float) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetScrollCallback(window.id,
        (_, xoffset, yoffset) =>
          unsafeRunF(
            getScale(window).flatMap((scaleX, scaleY) => callback(xoffset.toFloat * scaleX, yoffset.toFloat * scaleY))
          )
      )
      if old != null then
        old.free()
      end if
  end scrollCallback

  override def cursorPosCallback(window: OglGlfwWindow)(callback: (newPos: Point2d[Float]) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetCursorPosCallback(window.id, (_, xCursorPosition, yCursorPosition) =>
        unsafeRunF(getScale(window).flatMap((scaleX, scaleY) => callback(Point2d(xCursorPosition.toFloat * scaleX, yCursorPosition.toFloat * scaleY))))
      )
      if old != null then
        old.free()
      end if
  end cursorPosCallback

  override def mouseButtonCallback(window: OglGlfwWindow)(callback: (key: Int, action: KeyAction, mode: KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetMouseButtonCallback(window.id, (_, key, action, modes) =>
        unsafeRunF(callback(key, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end mouseButtonCallback

  override def currentMousePosition(window: OglGlfwWindow): F[Point2d[Float]] =
    getScale(window).flatMap:
      (scaleX, scaleY) =>
        stackPush(impure).use:
          stack =>
            impure.delay:
              val x = stack.mallocDouble(1)
              val y = stack.mallocDouble(1)
              glfwGetCursorPos(window.id, x, y)
              Point2d(x.get(0).toFloat * scaleX, y.get(0).toFloat * scaleY)
  end currentMousePosition

  override def swapBuffers(window: OglGlfwWindow): F[Unit] =
    impure.delay:
      glfwSwapBuffers(window.id)
  end swapBuffers

  override def makeContextCurrent(window: OglGlfwWindow): F[Unit] =
    impure.delay(
      glfwMakeContextCurrent(window.id)
    )
  end makeContextCurrent
end OglWindowIsGlfwWindow

