package gui4s.android.example

import android.app.Activity

import cats.effect.IO
import cats.effect.std.Supervisor
import cats.effect.kernel.Resource
import cats.effect.std.Queue
import gui4s.android.kit.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.android.kit.widgets.decorator.*
import gui4s.android.skia.*
import gui4s.android.skia.shaper.*
import gui4s.core.layout.Sized
import gui4s.core.geometry.Rect
import gui4s.core.widget.library.decorator.Paddings
import org.jetbrains.skia.{Font, Paint, Typeface}
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory

class MainActivity extends Gui4sActivity:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  def downloadImage: IO[android.graphics.drawable.Drawable] = //Image] =
    liftCallbackIOToAppIO(
      IO.delay:
        val res = getResources
        res.getDrawable(gui4s.android.example.R.drawable.gemma)//.toSkiaImage()
    )
  end downloadImage

  override def main(eventBus: Queue[IO, DownEvent]): Resource[IO, AndroidWidget[Nothing]] =
    for
      supervisor <- Supervisor[IO]
      shaper <- createShaper[IO]
      cache: TextCache[IO] <- RefCache.of[IO, (String, SkijaTextStyle, Option[Float]), Sized[Rect[Float], SkijaPlacedText]].eval
      paint =
        val res = new Paint()
        res.setColor(0xFF8484A4)
        res
      resource = ResourceWidget(
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer.andThen(liftCallbackIOToAppIO(_))
      )
      initialization = InitializationWidget(resource)
      text = textWidget(shaper, cache)
    yield
      initialization(
        name = "image",
        effectToRun = downloadImage,
        body = data => androidDrawableWidget[Nothing](data).clip(Shape.roundedCorners(80f)),
        placeholder = text("Wait.", SkijaTextStyle(new Font(Typeface.Companion.makeEmpty(), 28), paint)),
      ).padding(
        Paddings(10f, 10f, 10f, 10f)
      )
  end main

  /*
  override def createSkiaLayer =
    val skiaLayer = SkiaLayer()
    skiaLayer.setRenderDelegate(
      SkiaLayerRenderDelegate(
        skiaLayer,
        SimpleDrawableRenderer(
          skiaLayer,
          getResources.getDrawable(gui4s.android.example.R.drawable.gemma)
        )
      )
    )
    skiaLayer.needRedraw()
    skiaLayer*/
end MainActivity
