package me.katze.gui4s.glfw

import catnip.ForeighFunctionInterface
import cats.effect.Sync
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Point2d, Rect}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.MemoryUtil

final case class OglGlfwWindow[F[_] : Sync](
                                              id : Long,
                                              impure : ForeighFunctionInterface[F],
                                              unsafeRunF: [A] => F[A] => A
                                            ) extends GlfwWindow[F, Long, Float]:
  override def center: F[Unit] =
    size.flatMap:
      case Rect(width, height) =>
        impure.delay:
          val monitor = glfwGetPrimaryMonitor()
          val vidmode = glfwGetVideoMode(monitor) // TODO raise error on null
          glfwSetWindowPos(
            id,
            (vidmode.width - width.toInt) / 2,
            (vidmode.height - height.toInt) / 2
          )
  end center

  override def windowResizeCallback(callback: Rect[Float] => F[Unit]): F[Unit] =
    impure.delay:
      glfwSetWindowSizeCallback(
        id,
        (_, width, height) =>
          unsafeRunF(getScale.flatMap((scaleX, scaleY) => callback(Rect(width * scaleX, height * scaleY))))
      )
  end windowResizeCallback

  override def makeVisible: F[Unit] =
    impure.delay:
      glfwShowWindow(id)
  end makeVisible

  override def shouldClose: F[Boolean] =
    impure.delay:
      glfwWindowShouldClose(id)
  end shouldClose

  override def markForBeingClosed: F[Unit] =
    impure.delay:
      glfwSetWindowShouldClose(id, true)
  end markForBeingClosed

  def getScale : F[(Float, Float)] =
    stackPush(impure).use:
      stack =>
        impure.delay:
          val width = stack.mallocFloat(1)
          val height = stack.mallocFloat(1)
          glfwGetWindowContentScale(id, width, height)
          (width.get(0), height.get(0))
  end getScale

  override def frameBufferResizeCallback(callback: Rect[Float] => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetFramebufferSizeCallback(id, (_, width, height) =>
        unsafeRunF(callback(Rect(width, height)))
      )
      if old != null then
        old.free()
      end if
  end frameBufferResizeCallback

  override def keyCallback(callback: (key : Int, scanCode : Int, keyAction : KeyAction, keyModes : KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetKeyCallback(id, (_, key, scancode, action, modes) =>
        unsafeRunF(callback(key, scancode, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end keyCallback

  override def monitor : F[Long] = // TODO a typed error
    impure.delay(glfwGetWindowMonitor(id)).ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end monitor

  override def size: F[Rect[Float]] =
    getScale.flatMap:
      (xScale, yScale) =>
        stackPush(impure).use:
          stack =>
            impure.delay({
              val width = stack.mallocInt(1)
              val height = stack.mallocInt(1)
              glfwGetWindowSize(id, width, height)
              Rect(width.get(0) * xScale, height.get(0) * yScale)
            })
  end size

  override def frameBufferSize: F[Rect[Float]] =
    stackPush(impure).use:
      stack =>
        impure.delay:
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetFramebufferSize(id, width, height)
          Rect(width.get(0), height.get(0))
  end frameBufferSize

  //TODO проверить, правда ли это скрин координаты, а не пиксели.
  override def scrollCallback(callback: (xoffset : Float, yoffset : Float) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetScrollCallback(id,
        (_, xoffset, yoffset) =>
          unsafeRunF[Unit](
            getScale.flatMap((scaleX, scaleY) => callback(xoffset.toFloat * scaleX, yoffset.toFloat * scaleY))
          )
      )
      if old != null then
        old.free()
      end if
  end scrollCallback

  override def cursorPosCallback(callback: (newPos : Point2d[Float]) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetCursorPosCallback(id, (_, xCursorPosition, yCursorPosition) => 
        unsafeRunF[Unit](getScale.flatMap((scaleX, scaleY) => callback(Point2d(xCursorPosition.toFloat * scaleX, yCursorPosition.toFloat * scaleY))))
      )
      if old != null then
        old.free()
      end if
  end cursorPosCallback

  override def mouseButtonCallback(callback: (key : Int, action : KeyAction, mode : KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetMouseButtonCallback(id, (_, key, action, modes) =>
        unsafeRunF(callback(key, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end mouseButtonCallback

  override def currentMousePosition: F[Point2d[Float]] =
    getScale.flatMap:
      (scaleX, scaleY) =>
        stackPush(impure).use:
          stack =>
            impure.delay:
              val x = stack.mallocDouble(1)
              val y = stack.mallocDouble(1)
              glfwGetCursorPos(id, x, y)
              Point2d(x.get(0).toFloat * scaleX, y.get(0).toFloat * scaleY)
  end currentMousePosition

  override def swapBuffers: F[Unit] =
    impure.delay:
      glfwSwapBuffers(id)
  end swapBuffers
end OglGlfwWindow

