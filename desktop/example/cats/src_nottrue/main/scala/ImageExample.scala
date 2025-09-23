object ImageExample extends IOApp

  final case class PreInit(
                            dispatcher: Dispatcher[IO],
                            globalSupervisor: Supervisor[IO],
                            shaper: Shaper,
                            globalTextCache: TextCache[IO],
                            raiseEvent : DownEvent => IO[Unit]
                          ) <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- backend.skija.createShaper
      cache: TextCache[IO] <- ScalacacheCache()
    yield PreInit(dispatcher, supervisor, shaper, cache, backend.raiseEvent)
  end preInit

  def preInit(backend: gui4s.desktop.kit.common.SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]): Resource[IO, PreInit] =
    for
      dispatcher
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
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
    given Typeable[IO[Unit]] = a => a match
      case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
      case _ => None

    def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
    end downloadImage

    initWidget(
      supervisor = preInit.globalSupervisor,
      raiseExternalEvent = preInit.raiseEvent
    )(
      name = "image",
      imageSource = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
      imageWidget = data => image(data).clip(Shapes.roundedCorners(15f)),
      placeholder = text(preInit.shaper, preInit.globalTextCache)("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))),
    ).gapPadding(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end ImageExample
