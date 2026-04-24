package gui4s.desktop.kit.asset

import catnip.resource.*
import catnip.syntax.all.given
import catnip.syntax.transformer.given
import cats.Monad
import cats.data.OptionT
import cats.effect.Sync
import io.github.humbleui.skija.Data
import io.github.humbleui.skija.svg.SVGDOM

import gui4s.desktop.skija.Image
import gui4s.desktop.skija.image.*
import gui4s.desktop.skija.makeDataFromBytes
import gui4s.desktop.skija.svg.*

def skijaDataFromJavaResourcesT[
  IO[_] : Sync,
  Resource[_] : {SyncResource, EvalC[IO], UseC[IO], Monad}
](
  fileName : String
) : OptionT[Resource, Data] =
  javaResourcesFileBytesT(fileName)
    .evalK
    .flatMap(makeDataFromBytes)
end skijaDataFromJavaResourcesT

def skijaDataFromJavaResources[
  IO[_] : Sync,
  Resource[_] : {SyncResource, EvalC[IO], UseC[IO], Monad}
](
  fileName : String
) : Resource[Option[Data]] =
  skijaDataFromJavaResourcesT(fileName).value
end skijaDataFromJavaResources

def svgFromJavaResourcesT[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO], EvalC[IO]}
](
  fileName : String
) : OptionT[Resource, SVGDOM] =
  skijaDataFromJavaResourcesT[IO, Resource](fileName)
    .flatMap(makeSvgFromData)
end svgFromJavaResourcesT

def svgFromJavaResources[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO], EvalC[IO]}
](
  fileName : String
) : Resource[Option[SVGDOM]] =
  svgFromJavaResourcesT(fileName).value
end svgFromJavaResources

def imageFromJavaResourcesT[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO], EvalC[IO]}
](
  fileName : String
) : OptionT[Resource, Image] =
  javaResourcesFileBytesT[IO, Resource](fileName)
    .evalK
    .flatMap(makeImageFromBytes)
end imageFromJavaResourcesT

def imageFromJavaResources[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO], EvalC[IO]}
](
  fileName : String
) : Resource[Option[Image]] =
  imageFromJavaResourcesT(fileName).value
end imageFromJavaResources
