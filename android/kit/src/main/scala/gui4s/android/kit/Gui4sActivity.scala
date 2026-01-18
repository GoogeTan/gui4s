package gui4s.android.kit

import android.app.Activity
import gui4s.android.kit.widgets.*
import gui4s.android.kit.effects.*
import cats.effect.syntax.all.*
import android.content.res.Configuration
import android.util.Log
import android.widget.FrameLayout
import cats.effect.unsafe.implicits.global
import gui4s.core.geometry.Rect
import gui4s.core.widget.Path
import org.jetbrains.skiko.*
import org.jetbrains.skia.*
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable

final case class AppState[AppIO[_], CallbackIO[_]](
  dispatcher: Dispatcher[AppIO],
  eventBus : Queue[CallbackIO, DownEvent],
  widgetRef : Ref[AppIO, AndroidPlacedWidget[AppIO, Nothing]],
  layer : SkiaLayer
)

enum UIAppError:
  case PlaceError(exception : Throwable)

  def toThrowable : Throwable = this match
    case UIAppError.PlaceError(exception) =>
      Exception(s"Place error: $exception")
  end toThrowable

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
  final var state : Option[(AppState[AppIO, CallbackIO], AppIO[Unit])] = None

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

  final def runAppIOAsyncUnsafe[T](io : AppIO[T])(callback: Either[Throwable, T] => Unit) : Unit=
    io.value.unsafeRunAsync {
      case Left(error) =>
        callback(Left(error))
      case Right(Left(error)) =>
        callback(Left(error.toThrowable))
      case Right(Right(value)) =>
        callback(Right(value))
    }(using global)
  end runAppIOAsyncUnsafe

  final def getConfiguration : AppIO[AndroidConfiguration[Bounds]] =
    liftCallbackIOToAppIO(
      IO.delay(getResources.getConfiguration).map(AndroidConfiguration.fromJava)
    )
  end getConfiguration

  final def runPlaceK : PlaceC[AppIO] ~> AppIO =
    Place.run[AppIO](Path(Nil), getConfiguration)
      .andThen(mapErrorK(UIAppError.PlaceError(_)))
      .andThen(flattenEitherTK)
  end runPlaceK

  final def createEventBus : AppIO[Queue[CallbackIO, DownEvent]] =
    liftCallbackIOToAppIO(Queue.unbounded[CallbackIO, DownEvent])
  end createEventBus

  final def createWidgetRef(
    eventBus : Queue[CallbackIO, DownEvent]
  ) : Resource[AppIO, Ref[AppIO, AndroidPlacedWidget[AppIO, Nothing]]] =
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

  final def createState : Resource[AppIO, AppState[AppIO, CallbackIO]] =
    for
      dispatcher <- Dispatcher.parallel[AppIO]
      supervisor <- Supervisor[AppIO]
      eventBus <- createEventBus.eval
      _ = Log.e("Gui4sActivity", "onCreate created base")
      widgetRef : Ref[AppIO, AndroidPlacedWidget[AppIO, Nothing]] <- createWidgetRef(eventBus)
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
    yield AppState(dispatcher, eventBus, widgetRef, createSkiaLayer)
  end createState

  final def createSkiaLayer : SkiaLayer =
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

  final def setGui4sContent(state : AppState[AppIO, CallbackIO]) : Unit =
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
