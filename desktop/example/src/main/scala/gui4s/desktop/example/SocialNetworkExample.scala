package gui4s.desktop.example

import catnip.syntax.all.given
import cats.Id
import cats.data.EitherT
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import glfw4s.core._
import glfw4s.core.pure._
import glfw4s.jna.bindings.types._
import io.github.humbleui.skija.BlendMode

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import gui4s.core.geometry._
import gui4s.core.kit.ContainerPlacementError

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.skija._
import gui4s.desktop.skija.image.makeDeferredImageFromEncodedBytes
import gui4s.desktop.skija.typeface._
import gui4s.desktop.skija.shaper._

object SocialNetworkExample extends UIApp:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  override val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Social Network User Page",
    width = 800,
    height = 600,
  )

  // -- Data Models --
  case class User(name: String, status: String, avatarUrl: String)
  case class Post(author: String, content: String, imageUrl: Option[String])

  // -- Helpers for Clean Layout Syntax --
  def row[F[_]: Sync, Event](children: List[DesktopWidget[F, Event]], gap: Float = 0f): DesktopWidget[F, Event] = {
    val beginList: LinearContainerPlacementStrategy[F, List] =
      LinearContainerPlacementStrategy.Begin[F, List](gap)
    val beginOne: OneElementLinearContainerPlacementStrategy[F] =
      LinearContainerPlacementStrategy.Begin[F, Id](0f)

    rowWidget(children, beginList, beginOne)
  }

  def column[F[_]: Sync, Event](children: List[DesktopWidget[F, Event]], gap: Float = 0f): DesktopWidget[F, Event] = {
    val beginList: LinearContainerPlacementStrategy[F, List] =
      LinearContainerPlacementStrategy.Begin[F, List](gap)
    val beginOne: OneElementLinearContainerPlacementStrategy[F] =
      LinearContainerPlacementStrategy.Begin[F, Id](0f)

    columnWidget(children, beginList, beginOne)
  }

  // -- Image Downloading --
  def downloadImage[F[_]: Async](client: Client[F], uri: String): F[Image] =
    EitherT(
       Uri.fromString(uri).pure[F]
    ).flatMap(parsedUri =>
        EitherT.liftF(
           client.expect[Array[Byte]](parsedUri).map(makeDeferredImageFromEncodedBytes)
        )
    ).value.flatMap {
        case Right(img) => img.pure[F]
        case Left(_) => Sync[F].raiseError(new Exception(s"Invalid URI: $uri"))
    }

  // -- Widgets --
  // Abstracted widgets taking dependencies

  def avatarWidget[F[_]: Async, Event](url: String, client: Client[F], placeholder: DesktopWidget[F, Event])
                                      (using init: InitializationWidget[F, Event, Image]): DesktopWidget[F, Event] =
     init(
        name = url,
        effectToRun = downloadImage(client, url),
        body = image =>
             imageWidget[F, Event](image)
               .clip(Shapes.round),
        placeholder = placeholder
     )

  def userInfoWidget[F[_]: Sync, Event](user: User,
                                        nameText: String => DesktopWidget[F, Event],
                                        statusText: String => DesktopWidget[F, Event]): DesktopWidget[F, Event] =
    column(
      List(
        nameText(user.name),
        statusText(user.status)
      ),
      gap = 10f
    )

  def userProfileWidget[F[_]: Sync, Event](avatar: DesktopWidget[F, Event],
                                           userInfo: DesktopWidget[F, Event]): DesktopWidget[F, Event] =
    row(
      List(
        avatar,
        userInfo
      ),
      gap = 20f
    )

  def postWidget[F[_]: Sync, Event](post: Post,
                                    authorText: String => DesktopWidget[F, Event],
                                    contentText: String => DesktopWidget[F, Event],
                                    imageParams: Option[(String, DesktopWidget[F, Event])] // url and placeholder
                                   )
                                   (using
                                     mkAvatar: (String, DesktopWidget[F, Event]) => DesktopWidget[F, Event]
                                   ): DesktopWidget[F, Event] =
    column(
        List(
            authorText(post.author),
            contentText(post.content)
        ) ++ post.imageUrl.zip(imageParams).map { case (url, (pUrl, placeholder)) =>
             mkAvatar(url, placeholder)
        }.toList,
        gap = 5f
    )

  def feedWidget[F[_]: Sync, Event](posts: List[DesktopWidget[F, Event]]): DesktopWidget[F, Event] =
    column(
        posts,
        gap = 20f
    )

  def userPage[F[_]: Sync, Event](profile: DesktopWidget[F, Event], feed: DesktopWidget[F, Event]): DesktopWidget[F, Event] =
    column(
        List(
            profile,
            feed
        ),
        gap = 30f
    )

  override def main(
                     glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                     window: GLFWwindow,
                     eventBus: Queue[IO, DownEvent],
                   ) : Resource[AppIO, DesktopWidget[AppIO, Nothing]] =
    for
      dispatcher <- Dispatcher.sequential[AppIO]
      supervisor <- Supervisor[AppIO]
      client <- EmberClientBuilder.default[AppIO].build

      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      textWidgetFactory = TextWidget(shaper, cache)

      resource = ResourceWidget(supervisor, eventBus.offer.andThen(liftCallbackIOToAppIO(_)))
      given InitializationWidget[AppIO, Nothing, Image] = InitializationWidget(resource)

      typeface <- defaultTypeface[AppIO]
      nameStyle = SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF000000))
      statusStyle = SkijaTextStyle(new Font(typeface, 16), new Paint().setColor(0xFF888888))
      postContentStyle = SkijaTextStyle(new Font(typeface, 14), new Paint().setColor(0xFF000000))

      // Dummy Data
      user = User("Jane Doe", "Software Engineer | Scala Enthusiast", "https://i.pravatar.cc/150?u=jane")
      posts = List(
        Post("Jane Doe", "Just started learning gui4s! It's amazing.", None),
        Post("Jane Doe", "Here is a cool picture.", Some("https://picsum.photos/200/300"))
      )

    yield
       // Instantiate concrete widgets
       val placeholderText = textWidgetFactory("Loading...", statusStyle)

       def mkAvatar(url: String, placeholder: DesktopWidget[AppIO, Nothing]): DesktopWidget[AppIO, Nothing] =
          avatarWidget(url, client, placeholder)

       val profile = userProfileWidget(
          mkAvatar(user.avatarUrl, placeholderText),
          userInfoWidget(
             user,
             name => textWidgetFactory(name, nameStyle),
             status => textWidgetFactory(status, statusStyle)
          )
       )

       val feed = feedWidget(
          posts.map { post =>
             // We need to pass the placeholder if image exists. In this simple example we use same placeholder.
             // We also need to adapt postWidget which is generic.
             given (String, DesktopWidget[AppIO, Nothing]) => DesktopWidget[AppIO, Nothing] = mkAvatar

             postWidget(
                post,
                author => textWidgetFactory(author, nameStyle),
                content => textWidgetFactory(content, postContentStyle),
                post.imageUrl.map(url => (url, placeholderText))
             )
          }
       )

       userPage(profile, feed)

  end main

end SocialNetworkExample
