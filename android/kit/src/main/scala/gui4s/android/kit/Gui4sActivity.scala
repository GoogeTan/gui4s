package gui4s.android.kit

import android.app.Activity
import gui4s.android.kit.widgets.*
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.*
import cats.effect.syntax.all.*

import scala.reflect.Typeable
import android.content.res.Configuration
import android.os.Looper
import android.util.Log
import android.view.Gravity
import android.view.ViewGroup.LayoutParams
import android.widget.{FrameLayout, TextView}
import cats.effect.unsafe.implicits.global
import gui4s.core.loop.updateLoop
import gui4s.core.widget.Path
import org.jetbrains.skiko.*
import org.jetbrains.skia.*
import gui4s.desktop.widget.library.*

final case class AppState[AppIO[_], CallbackIO[_]](
  dispatcher: Dispatcher[AppIO],
  eventBus : Queue[CallbackIO, DownEvent],
  widgetRef : Ref[AppIO, AndroidPlacedWidget[AppIO, Nothing]]
)

enum UIAppError:
  case PlaceError(exception : Throwable)

  override def toString: String =
    this match
      case PlaceError(exception) => "PlaceError(" + exception + ")"
  end toString
end UIAppError

trait Gui4sActivity extends Activity:
  final type AppIO[T] = EitherT[IO, UIAppError, T]

  final given Typeable[AppIO[Unit]] = (a : Any) =>
    a match
      case _ : EitherT[io, b, c] =>
        Some(a.asInstanceOf[EitherT[IO, UIAppError, Unit] & a.type])
      case _ => None
    end match
  end given

  final type CallbackIO[T] = IO[T]
  final val liftCallbackIOToAppIO : CallbackIO ~> AppIO = EitherT.liftK
  var state : Option[(AppState[AppIO, CallbackIO], AppIO[Unit])] = None

  override def onCreate(savedInstanceState: android.os.Bundle): Unit =
    super.onCreate(savedInstanceState)
    Log.d("Gui4sActivity", s"onCreate started on ${Thread.currentThread().getId}")
    setErrorView("Loading")
    try
      (
        for
          dispatcher <- Dispatcher.parallel[AppIO]
          supervisor <- Supervisor[AppIO]
          eventBus <- liftCallbackIOToAppIO(Queue.unbounded[CallbackIO, DownEvent]).eval
          getConfigutation = IO.delay(getResources.getConfiguration).map(AndroidConfiguration.fromJava)
          runPlaceK : (PlaceC[AppIO] ~> AppIO) =
            Place.run[AppIO](Path(Nil), liftCallbackIOToAppIO(getConfigutation))
              .andThen(mapErrorK(UIAppError.PlaceError(_)))
              .andThen(flattenEitherTK)
          _ = Log.e("Gui4sActivity", "onCreate created base")
          widgetRef : Ref[AppIO, AndroidPlacedWidget[AppIO, Nothing]] <- main(eventBus).evalMap(freeMainWidget =>
            Ref.ofEffect(runWidgetForTheFirstTime(freeMainWidget, runPlaceK, RecompositionReaction.run))
          )
          _ = Log.e("Gui4sActivity", "onCreate created widget")
          _ <- supervisor.supervise(
            widgetRef.get.flatMap(
              androidWidgetLoops[AppIO, Nothing](
                Update.runUpdate[AppIO, Nothing],
                runPlaceK,
              )(_, newWidget => widgetRef.update(_ => newWidget), liftCallbackIOToAppIO(eventBus.take))
            )
          ).eval
          _ = Log.e("Gui4sActivity", "onCreate created loop")
        yield AppState(dispatcher, eventBus, widgetRef)
      )
        .allocated
        .value
        .unsafeRunAsync {
          case Left(error) =>
            Log.e("Gui4sActivity", s"Error creating app: \n${error.toString}")
            state = None
            setErrorView(s"Error while creating app: \n${error.toString}")
          case Right(Left(error)) =>
            Log.e("Gui4sActivity", s"Error creating app: \n${error.toString}")
            state = None
            setErrorView(s"Error while creating app: \n${error.toString}")
          case Right(Right((state, destructor))) =>
            Log.e("Gui4sActivity", "Successfully created state")
            AndroidMainThread.execute(() => {
                this.state = Some((state, destructor))
                setGui4sContent()
              }
            )
            Log.e("Gui4sActivity", "Successfully inited")
        }(using global)
    catch
      case e : Throwable =>
        Log.e("Gui4sActivity", s"Error creating app: \n${e.toString}")
        setErrorView(s"Exception while creating app: \n${e.toString}")
    end try
    Log.e("Gui4sActivity", "onCreate finished")
  end onCreate

  def redraw : IO[Unit] =
    IO.delay:
      ()

  def setGui4sContent() : Unit =
    val container = FrameLayout(this)
    setContentView(container)

    val skiaLayer = SkiaLayer()
    skiaLayer.setRenderDelegate(
      SkiaLayerRenderDelegate(
        skiaLayer,
        RenderDelegate()
      )
    )
    skiaLayer.attachTo(container)
    skiaLayer.needRedraw()
  end setGui4sContent

  def setErrorView(error : String) : Unit =
    val textView = new TextView(this)
    textView.setText(error)
    textView.setTextSize(16)
    textView.setTextColor(Color.RED)
    textView.setGravity(Gravity.TOP)
    textView.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT))
    setContentView(textView)
  end setErrorView

  override def onConfigurationChanged(newConfig: Configuration): Unit =
    super.onConfigurationChanged(newConfig)
  end onConfigurationChanged

  def main(
    eventBus: Queue[IO, DownEvent],
  ): Resource[AppIO, AndroidWidget[AppIO, Nothing]]

  final class RenderDelegate extends SkikoRenderDelegate:
    override def onRender(
      canvas : Canvas,
      width : Int,
      height : Int,
      nanos : Long,
    ) : Unit =
      state match
        case Some((state, _)) =>
          state.dispatcher.unsafeRunAndForget(
            state
              .widgetRef
              .get
              .flatMap(_.draw.run(canvas))
              .evalOn(AndroidMainThread)
          )
        case None =>
          ???
      end match
    end onRender
  end RenderDelegate
end Gui4sActivity
