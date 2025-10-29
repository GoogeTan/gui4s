package gui4s.desktop.kit

import catnip.syntax.all.{*, given}
import cats.*
import cats.data.{EitherT, ReaderT}
import cats.effect.*
import cats.effect.std.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import glfw4s.core.impure.SafeImpurePostInit
import glfw4s.core.pure.*
import glfw4s.core.types.GlfwError
import glfw4s.core.{CatsPostInit, WindowCreationSettings}
import glfw4s.jna.bindings.types.{GLFWmonitor, GLFWwindow}
import glfw4s.jvm.{JvmImpurePostInit, JvmImpurePreInit}
import gui4s.core.geometry.*
import gui4s.core.widget.*
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.skija.DirectContext.*
import gui4s.desktop.skija.canvas.clear
import gui4s.desktop.widget.library.*
import io.github.humbleui.skija.Canvas

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
      glfw <- CatsPostInit(
        JvmImpurePreInit,
        SafeImpurePostInit(JvmImpurePostInit),
        MainThread,
        error => EitherT.leftT[CallbackIO, Unit](UIAppError.InitError(error))
      )()
      eventBus <- Queue.unbounded[CallbackIO, DownEvent].to[AppIO].eval
      window <- glfw.createWindow(settings)
      _ <- glfw.makeContextCurrent(window).eval
      surface <- SkijaSurface.create(window, glfw, liftCallbackIOToAppIO).evalOn(MainThread)
      _ <- glfw.addFramebufferSizeCallback(
        window,
        (_ : GLFWwindow, w : Int, h : Int) =>
          surface.recreateRenderTarget(Rect(w, h))
            *> eventBus.offer(DownEvent.WindowResized)
      ).eval

      runPlaceK : (PlaceC[AppIO] ~> AppIO) =
        Place.run[AppIO](windowBounds(window, glfw))
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
              _ <- glfw.pollEvents()
            yield (),
            liftCallbackIOToAppIO
          ) *> glfw.shouldNotWindowClose(window)

      widgetCell <- main(glfw, window, eventBus).evalMap(freeMainWidget =>
        Ref.ofEffect(runWidgetForTheFirstTime(freeMainWidget, runPlaceK))
      )
      _ <- Console[AppIO].println("Starting the main loop").eval
      exitCode <- desktopWidgetLoops[AppIO, CallbackIO](
        runDraw =  runDrawK,
        runPlace = runPlaceK,
        waitForTheNextEvent = liftCallbackIOToAppIO(eventBus.take),
        updateLoopExecutionContext = this.runtime.compute,
        drawLoopExecutionContext = MainThread,
        widget = widgetCell,
      ).eval
    yield exitCode
  end runResourced

  def windowBounds(window: GLFWwindow, glfw : PostInit[AppIO, CallbackIO[Unit], GLFWmonitor, GLFWwindow]): AppIO[Bounds] =
    glfw.getFramebufferSize(window).map((w, h) => Rect(new InfinityOr(w.toFloat), new InfinityOr(h.toFloat)))
  end windowBounds

  final def runWidgetForTheFirstTime(
                                      widget: DesktopWidget[AppIO, ApplicationRequest],
                                      runPlace : PlaceC[AppIO] ~> AppIO,
                                    ): AppIO[DesktopPlacedWidget[AppIO, ApplicationRequest]] =
    placeForTheFirstTime[AppIO, DesktopPlacedWidget[AppIO, ApplicationRequest], PlaceC[AppIO], RecompositionReaction[AppIO]](
      Path(Nil),
      widget,
      widgetReactsOnRecomposition[
        UpdateC[AppIO, ApplicationRequest],
        PlaceC[AppIO],
        Draw[AppIO],
        RecompositionReaction[AppIO],
        DownEvent,
      ],
      RecompositionReaction.run[AppIO],
      runPlace,
    )
  end runWidgetForTheFirstTime

  def main(
            glfw: PostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
  ): Resource[AppIO, DesktopWidget[AppIO, ApplicationRequest]]
end UIApp