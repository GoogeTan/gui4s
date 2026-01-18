package gui4s.desktop.kit

import scala.reflect.Typeable

import catnip.syntax.all.{_, given}
import cats._
import cats.data.EitherT
import cats.data.ReaderT
import cats.effect._
import cats.effect.std._
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure._
import glfw4s.core.types.GlfwError
import glfw4s.jna.bindings.structs.GLFWcursor
import glfw4s.jna.bindings.types.GLFWmonitor
import glfw4s.jna.bindings.types.GLFWwindow
import glfw4s.jvm.CatsJvmPostInit
import io.github.humbleui.skija.Canvas

import gui4s.core.geometry._
import gui4s.core.widget._

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.skija.DirectContext._
import gui4s.desktop.skija.canvas.clear
import gui4s.desktop.widget.library._

enum UIAppError:
  case InitError(glfwError : GlfwError)
  case PlaceError(exception : Throwable)

  override def toString: String =
    this match
      case InitError(GlfwError(code, description, trace)) => "InitError(" + code + ", " + description + ", " + trace + ")"
      case PlaceError(exception) => "PlaceError(" + exception + ")"
  end toString
end UIAppError

trait UIApp extends IOApp:
  final type AppIO[T] = EitherT[IO, UIAppError, T]

  final given Typeable[AppIO[Unit]] = (a : Any) =>
    a match
      case _ : EitherT[io, b, c] =>
        Some(a.asInstanceOf[EitherT[IO, UIAppError, Unit] & a.type])
      case _ => None
    end match
  end given

  final type CallbackIO[T] = IO[T]
  final val liftCallbackIOToAppIO : (CallbackIO ~> AppIO) = EitherT.liftK

  val settings : WindowCreationSettings[GLFWmonitor, GLFWwindow]

  final override def run(args: List[String]): IO[ExitCode] =
    runResourced
      .use(EitherT.pure(_))
      .foldF(
        fa = error => IO.raiseError(new Exception(error.toString)),
        fb = IO.pure[ExitCode],
      )
  end run

  final def runResourced : Resource[AppIO, ExitCode] =
    given IORuntime = runtime
    for
      glfw <- CatsJvmPostInit(
        MainThread,
        error => EitherT.leftT[CallbackIO, Unit](UIAppError.InitError(error))
      )()
      eventBus <- Queue.unbounded[CallbackIO, DownEvent].to[AppIO].eval
      window <- glfw.createWindow(settings)
      _ <- glfw.makeContextCurrent(Some(window)).eval
      surface <- SkijaSurface.create(window, glfw, liftCallbackIOToAppIO).evalOn(MainThread)
      _ <- glfw.addFramebufferSizeCallback(
        window,
        (_ : GLFWwindow, w : Int, h : Int) =>
          surface.recreateRenderTarget(Rect(w, h))
            *> eventBus.offer(DownEvent.WindowShouldBeRedrawn)
      ).eval

      runPlaceK : (PlaceC[AppIO] ~> AppIO) =
        Place.run[AppIO](Path(Nil), windowBounds(window, glfw))
          .andThen(mapErrorK(UIAppError.PlaceError(_)))
          .andThen(flattenEitherTK)

      runDrawK : (Draw[AppIO] => AppIO[Boolean]) =
        draw =>
          surface.drawFrame(sur =>
            for
              _ <- clear[ReaderT[AppIO, Canvas, *]](0xFFFFFFFF).run(sur.canvas)
              _ <- draw.run(sur.canvas)
              _ <- flush[AppIO](sur.directContext)
              _ <- glfw.swapBuffers(window)
              _ <- glfw.pollEvents
            yield (),
            liftCallbackIOToAppIO
          ) *> glfw.windowShouldClose(window).map(!_)

      widgetCell <- main(glfw, window, eventBus).evalMap(freeMainWidget =>
        Ref.ofEffect(runWidgetForTheFirstTime(freeMainWidget, runPlaceK, RecompositionReaction.run))
      )
      exitCode <- desktopWidgetLoops[AppIO, CallbackIO, Nothing](
        runDraw =  runDrawK,
        runPlace = runPlaceK,
        waitForTheNextEvent = liftCallbackIOToAppIO(eventBus.take),
        updateLoopExecutionContext = this.runtime.compute,
        drawLoopExecutionContext = MainThread,
        widget = widgetCell,
      ).eval
    yield exitCode
  end runResourced

  def windowBounds(window: GLFWwindow, glfw : PureWindow[AppIO, CallbackIO[Unit], GLFWmonitor, GLFWwindow]): AppIO[Bounds] =
    glfw.getFramebufferSize(window).map((w, h) => Rect(new InfinityOr(w.toFloat), new InfinityOr(h.toFloat)))
  end windowBounds

  def main(
            glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
  ): Resource[AppIO, DesktopWidget[AppIO, Nothing]]
end UIApp