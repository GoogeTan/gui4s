package gui4s.desktop.kit.asset

import java.io.InputStream

import catnip.resource.*
import catnip.resource.WrapStrategy.Blocking
import catnip.syntax.resource.given
import cats.Monad
import cats.data.*
import cats.effect.Sync

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
def javaResourcesInputStream[Resource[_] : {SyncResource as S}](fileName: String): Resource[Option[InputStream]] =
  S.make(
    {
      val path = if (fileName.startsWith("/")) fileName.drop(1) else fileName
      val thread = Thread.currentThread
      val classLoader = Option(thread.getContextClassLoader).getOrElse(fileName.getClass.getClassLoader)
      val res = classLoader.getResourceAsStream(path)
      if res == null then
        (None, () => ())
      else
        (Some(res), () => res.close())
    },
    allocateStrategy = Blocking,
    deallocateStrategy = Blocking
  )
end javaResourcesInputStream

def javaResourcesInputStreamT[Resource[_] : SyncResource](fileName: String): OptionT[Resource, InputStream] =
  OptionT(javaResourcesInputStream(fileName))
end javaResourcesInputStreamT  

def javaResourcesFileBytesT[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO]}
](fileName: String): OptionT[IO, Array[Byte]] =
  javaResourcesInputStreamT[Resource](fileName)
    .use(readStreamBytes[OptionT[IO, *]])
end javaResourcesFileBytesT

def javaResourcesFileBytes[
  IO[_] : Sync,
  Resource[_] : {SyncResource, Monad, UseC[IO]}
](fileName: String): IO[Option[Array[Byte]]] =
  javaResourcesFileBytesT(fileName).value
end javaResourcesFileBytes