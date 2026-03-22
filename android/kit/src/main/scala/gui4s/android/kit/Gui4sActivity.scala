package gui4s.android.kit

import android.app.Activity
import cats.effect.IO
import cats.effect.std.{Dispatcher, Queue, Supervisor}
import cats.effect.kernel.{Ref, Resource}
import cats.~>
import catnip.syntax.all.*
import gui4s.core.kit.effects.RecompositionReaction
import gui4s.android.kit.widgets.*
import gui4s.android.kit.effects.*
import android.content.res.Configuration
import android.util.Log
import android.widget.FrameLayout
import cats.effect.unsafe.implicits.global
import gui4s.core.widget.Path
import org.jetbrains.skiko.*
import org.jetbrains.skia.*
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable

final case class AppState(
  dispatcher: Dispatcher[IO],
  eventBus : Queue[IO, DownEvent],
  widgetRef : Ref[IO, AndroidPlacedWidget[Nothing]],
  layer : SkiaLayer
)

trait Gui4sActivity extends Activity:
  final given Typeable[IO[Unit]] = (a : Any) =>
    a match
      case _ : IO[t] =>
        Some(a.asInstanceOf[IO[Unit] & a.type])
      case _ => None
    end match
  end given

  final val liftCallbackIOToAppIO : IO ~> IO = FunctionK.id
  final var state : Option[(AppState, IO[Unit])] = None

  override def onCreate(savedInstanceState: android.os.Bundle): Unit =
    super.onCreate(savedInstanceState)
    Log.d("Gui4sActivity", s"onCreate started on ${Thread.currentThread().getId}")
    setLoadingView()
    try
      runAppIOAsyncUnsafe(createState.allocated) {
        result =>
          AndroidMainThread.execute(
            () => {
              result match
                case Left(error) =>
                  Log.e("Gui4sActivity", s"Error creating app: \n${error.toString}")
                  state = None
                  setErrorView(s"Error while creating app: \n${error.toString}")
                case Right((state, destructor)) =>
                  Log.e("Gui4sActivity", "Successfully created state")
                  this.state = Some((state, destructor))
                  setGui4sContent(state)
                  Log.e("Gui4sActivity", "Successfully inited")
            }
          )
      }
    catch
      case e : Throwable =>
        Log.e("Gui4sActivity", s"Error creating app: \n${e.toString}")
        setErrorView(s"Exception while creating app: \n${e.toString}")
    end try
    Log.e("Gui4sActivity", "onCreate finished")
  end onCreate

  final def runAppIOAsyncUnsafe[T](io : IO[T])(callback: Either[Throwable, T] => Unit) : Unit=
    io.unsafeRunAsync {
      case Left(error) =>
        callback(Left(error))
      case Right(value) =>
        callback(Right(value))
    }(using global)
  end runAppIOAsyncUnsafe

  final def getConfiguration : IO[AndroidConfiguration[Bounds]] =
    liftCallbackIOToAppIO(
      IO.delay(getResources.getConfiguration).map(AndroidConfiguration.fromJava)
    )
  end getConfiguration

  final def runPlaceK : PlaceC ~> IO =
    Place.run(Path(Nil), getConfiguration)
  end runPlaceK

  final def createEventBus : IO[Queue[IO, DownEvent]] =
    liftCallbackIOToAppIO(Queue.unbounded[IO, DownEvent])
  end createEventBus

  final def createWidgetRef(
    eventBus : Queue[IO, DownEvent]
  ) : Resource[IO, Ref[IO, AndroidPlacedWidget[Nothing]]] =
    main(eventBus).evalMap(freeMainWidget =>
      Ref.ofEffect(
        runWidgetForTheFirstTime(
          freeMainWidget,
          runPlaceK,
          RecompositionReaction.run
        ).evalOn(AndroidMainThread)
      )
    )
  end createWidgetRef

  final def createState : Resource[IO, AppState] =
    for
      dispatcher <- Dispatcher.parallel[IO]
      supervisor <- Supervisor[IO]
      eventBus <- createEventBus.eval
      _ = Log.e("Gui4sActivity", "onCreate created base")
      widgetRef : Ref[IO, AndroidPlacedWidget[Nothing]] <- createWidgetRef(eventBus)
      _ = Log.e("Gui4sActivity", "onCreate created widget")
      _ <- supervisor.supervise(
        widgetRef.get.flatMap(
          androidWidgetLoops[Nothing](
            Update.runUpdate[Nothing],
            runPlaceK,
          )(_, newWidget => widgetRef.update(_ => newWidget), liftCallbackIOToAppIO(eventBus.tryTakeN(None)))
        )
      ).eval
      _ = Log.e("Gui4sActivity", "onCreate created loop")
    yield AppState(dispatcher, eventBus, widgetRef, createSkiaLayer)
  end createState

  def createSkiaLayer : SkiaLayer =
    val layer = SkiaLayer()
    layer.setRenderDelegate(
      SkiaLayerRenderDelegate(
        layer,
        RenderDelegate()
      )
    )
    layer.needRedraw()
    layer
  end createSkiaLayer

  final def setGui4sContent(state : AppState) : Unit =
    val container = FrameLayout(this)
    setContentView(container)
    state.layer.attachTo(container)
  end setGui4sContent

  def setLoadingView() : Unit =
    setContentView(createBiDirectionalErrorView(this, "Loading"))
  end setLoadingView

  final def setErrorView(error : String) : Unit =
    setContentView(createBiDirectionalErrorView(this, error))
  end setErrorView

  override def onConfigurationChanged(newConfig: Configuration): Unit =
    super.onConfigurationChanged(newConfig)
  end onConfigurationChanged

  def main(
    eventBus: Queue[IO, DownEvent],
  ): Resource[IO, AndroidWidget[Nothing]]

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
