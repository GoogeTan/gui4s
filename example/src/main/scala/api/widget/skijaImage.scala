package me.katze.gui4s.example.api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.effect.IO
import cats.syntax.all.*
import cats.{Monad, Monoid}
import io.github.humbleui.skija.Image
import me.katze.gui4s.example.api.effects.{*, given}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.library.{Widget, WithContext, drawOnlyWidget}
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder

def downloadImage(uri: String): IO[Image] =
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

def image[
  IO[_] : Monad,
  Update[_] : Monad,
  RecompositionReaction : Monoid,
  DownEvent,
  PlaceError
](
  image: Image,
  ffi : ForeighFunctionInterface[IO]
) : SkijaPlace[IO, Float, PlaceError, Widget[Update, SkijaPlaceT[IO, Float, PlaceError], SkijaDraw[IO], RecompositionReaction, DownEvent]] =
  drawOnlyWidget[
    Update,
    SkijaPlaceT[IO, Float, PlaceError],
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
  ](
    Sized(drawImage(ffi, image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[SkijaOuterPlaceT[IO, Float, PlaceError]],
    Monoid[RecompositionReaction].empty,
  )
end image

def imageUrl[
  ImageSource,
  Widget,
  Image
](
  name : String,
  resourceInit : (String, ImageSource) => WithContext[Widget, Option[Image]],
  imageSource : ImageSource,
  imageWidget : Image => Widget,
  placeholder : Widget,
) : Widget =
  resourceInit(
    name,
    imageSource
  ) {
    case Some(image) => imageWidget(image)
    case None => placeholder  
  }
end imageUrl
