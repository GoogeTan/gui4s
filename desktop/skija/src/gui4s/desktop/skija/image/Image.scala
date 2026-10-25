package gui4s.desktop.skija.image

import catnip.resource.SyncResource
import io.github.humbleui.skija.Image

import gui4s.desktop.skija.Image

def makeImageFromBytes[Resource[_] : SyncResource as S](bytes : Array[Byte]) : Resource[Image] =
  S.fromAutoCloseable(Image.makeDeferredFromEncodedBytes(bytes))
end makeImageFromBytes
