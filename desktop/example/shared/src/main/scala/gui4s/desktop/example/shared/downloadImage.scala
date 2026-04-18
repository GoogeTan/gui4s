package gui4s.desktop.example.shared

import cats.effect.IO
import io.github.humbleui.skija.Image
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory

def downloadImage(uri: String): IO[Image] =
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger
  EmberClientBuilder
    .default[IO]
    .build
    .use(
      client =>
        IO.fromEither(
          Uri.fromString(uri)
        ).flatMap(
          client.expect[Array[Byte]]
        ).map(Image.makeDeferredFromEncodedBytes)
    )
end downloadImage
