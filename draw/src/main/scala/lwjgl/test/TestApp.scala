package me.katze.gui4s.draw
package lwjgl.test

import lwjgl.{Example, Truetype}

import cats.effect.std.Dispatcher
import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.{GL_COLOR_BUFFER_BIT, GL_FLOAT, GL_TRIANGLES, GL_VERTEX_ARRAY, glClear, glColor3f, glDisableClientState, glDrawArrays, glEnableClientState, glPopMatrix, glPushMatrix, glVertexPointer}
import org.lwjgl.system.MemoryUtil.memAllocFloat

object TestApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    mainIO.useForever.evalOn(MainThread) *> IO.pure(ExitCode.Success)
  end run

  def mainTest : IO[Unit] =
    IO:
      new Truetype("Hello from cats!").run("An amazing window")
  end mainTest

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  def mainIO : Resource[IO, Unit] =
    val vertices = Array(0.0f, 0.5f, 0.0f, -0.5f, -0.5f, 0.0f, 0.5f, -0.5f, 0.0f)
    val vertexBuffer = memAllocFloat(vertices.length)
    vertexBuffer.put(vertices).flip
    val fontHeight = 18
    for
      dispatcher <- Dispatcher.sequential[IO]
      window <- initWindowResource(
        dispatcher = dispatcher,
        title = "Test",
        windowWidth = 800,
        windowHeight = 800,
        keyCallback = (_, _, _, _, _) => IO.unit,
        scrollCallback = (_, _, _) => IO.unit
      )
      font <- Resource.eval(Font.load("JetBrainsMono-Regular.ttf").map(_.toOption.get))
      currentWindow <- Resource.eval(window.get)
      //bakedFont <- initBakedCharBuffer(font, currentWindow, fontHeight * currentWindow.contentScaleY)
      _ <- Resource.eval(
        drawLoop(
          window.get,
          IO:
            glPushMatrix()
            //renderText(currentWindow, bakedFont, "12345", font, fontHeight, false, true) 
            glPopMatrix()
            
            glColor3f(0.0f, 1.0f, 0.0f)
            glEnableClientState(GL_VERTEX_ARRAY)
            glVertexPointer(3, GL_FLOAT, 0, vertexBuffer)
            glDrawArrays(GL_TRIANGLES, 0, 3)
            glDisableClientState(GL_VERTEX_ARRAY)
        )
      )
    yield ()
  end mainIO
end TestApp
