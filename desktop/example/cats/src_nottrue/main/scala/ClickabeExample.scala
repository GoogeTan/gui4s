object ClickabeExample extends IOApp

  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[IO], mousePosition : IO[Point2d[Float]]) <- backend.skija.createShaper
      cache : TextCache[IO] <- ScalacacheCache()
    yield PreInit(shaper, cache, backend.mousePosition)
  end preInit

  def preInit(backend : gui4s.desktop.kit.common.SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Resource[IO, PreInit] =
    for
      shaper
  end run


  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher.sequential[IO].use(
      dispatcher =>
        desktopApp(
          preInit = preInit,
          main = main,
          updateLoopExecutionContext = this.runtime.compute,
          drawLoopExecutionContext = MainThread,
          settings = WindowCreationSettings(
            title = "Gui4s image widget example",
            size = Rect(620f, 480f),
            visible = true,
            resizeable = true,
            debugContext = true
          ),
          unsafeRunF = dispatcher.unsafeRunAndForget 
        )
    )
      def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
    extension (value: DesktopWidget[Unit])
    end extension

    statefulWidget[Int, ApplicationRequest, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[ApplicationRequest]],
      body = state =>
        text(
          preInit.shaper,
          preInit.globalTextCache
        )(
          "test text " + state.toString,
          SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
        ).onClick(())
    )
  end main
end ClickabeExample
