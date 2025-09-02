package gui4s.desktop.kit.zio

import effects.given
import widgets.DesktopWidget

import catnip.zio.{toZIO, zioScopedToResource}
import gui4s.desktop.kit.common.SkijaBackend
import gui4s.desktop.kit.common.effects.{ApplicationRequest, DownEvent}
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import zio.*
import zio.interop.catz.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

trait Gui4sZioApp extends MainThreadApp:
  type PreInit 
  
  val settings : WindowCreationSettings[Float]

  def preInit(backend : SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]) : RIO[Scope, PreInit]
  
  override def run(args: List[String]): ZIO[Any, Throwable, ExitCode] =
    gui4s.desktop.kit.common.desktopApp[Task, PreInit](
      preInit.andThen(zioScopedToResource),
      main,
      MainThreadApp.mainThread,
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10)),
      settings
    ).map(_.toZIO)
  end run
  
  def main(preInit : PreInit) : DesktopWidget[ApplicationRequest]
end Gui4sZioApp

