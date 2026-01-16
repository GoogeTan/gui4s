package gui4s.android.example

import android.app.Activity
import cats.effect.IO
import cats.effect.std.{Dispatcher, Supervisor}
import gui4s.android.kit.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.android.kit.widgets.decorator.*
import gui4s.android.skia.*
import gui4s.android.skia.canvas.*
import gui4s.android.skia.shaper.*
import gui4s.core.geometry
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.Paddings
import org.jetbrains.skia.{Font, Image, Paint, Typeface}
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory

class MainActivity extends Gui4sActivity:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  def downloadImage: AppIO[Image] =
    liftCallbackIOToAppIO(
      IO.delay:
        val res = getResources
        res.getDrawable(gui4s.android.example.R.drawable.cosmos).toSkiaImage()
    )
  end downloadImage

  override def main(eventBus: Queue[IO, DownEvent]): Resource[AppIO, AndroidWidget[AppIO, ApplicationRequest]] =
    extension[Event](value : AndroidWidget[AppIO, Event])
      def clip(shape : geometry.Rect[Float] => Clip) : AndroidWidget[AppIO, Event] =
        gui4s.android.kit.widgets.decorator.clip(value)(shape)
      end clip
    end extension

    for
      supervisor <- Supervisor[AppIO]
      shaper <- createShaper[AppIO]
      cache: TextCache[AppIO] <- RefCache.of[AppIO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]].eval
      paint =
        val res = new Paint()
        res.setColor(0xFF8484A4)
        res
    yield gapPadding[AppIO, ApplicationRequest](
      Paddings(10f, 10f, 10f, 10f)
    )(
      initWidget(
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer.andThen(liftCallbackIOToAppIO(_))
      )(
        name = "image",
        imageSource = downloadImage,
        imageWidget = data => image[AppIO, ApplicationRequest](data).clip(Shapes.round),
        placeholder = text(shaper, cache)("Wait.", SkijaTextStyle(new Font(Typeface.Companion.makeEmpty(), 28), paint)),
      )
    )
  end main
end MainActivity
