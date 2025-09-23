object GridExample extends IOApp

  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[IO]) <- backend.skija.createShaper
      cache : TextCache[IO] <- ScalacacheCache()
    yield PreInit(shaper, cache)
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
            title = "Gui4s nested containers example",
            size = Rect(620f, 480f),
            visible = true,
            resizeable = true,
            debugContext = true
          ),
          unsafeRunF = dispatcher.unsafeRunAndForget
        )
    )
    def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
      val def gridExample[Event](numbers : List[Int]) : DesktopWidget[Event] = : PlacementStrategy[OuterPlace, InfinityOr[Float], List, Float] = PlacementStrategy.ErrorIfInfinity(
        PlacementStrategy.SpaceBetween[OuterPlace, List, Float],
        ContainerPlacementError.English.withSpaceBetweenStrategy
      )
      val spaceBetween = OneElementPlacementStrategy.Begin[OuterPlace, InfinityOr[Float], Float]
      linearContainer[Event](
        mainAxis = Axis.Vertical,
        mainAxisStrategy = spaceBetween,
        additionalAxisStrategy = begin,
        children =
          numbers.map:
            lineIndex =>
              linearContainer[Event](
                mainAxis = Axis.Horizontal,
                mainAxisStrategy = spaceBetween,
                additionalAxisStrategy = begin,
                children =
                  numbers.map:
                    lineJindex =>
                      text(preInit.shaper, preInit.globalTextCache)(
                        lineIndex.toString + ":" + lineJindex.toString,
                        SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
                      )
              )
      )
    end gridExample

    gridExample((0 until 10).toList)
  end main
end GridExample
