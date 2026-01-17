package gui4s.desktop.skija.image

import io.github.humbleui.skija.Image

def makeDeferredImageFromEncodedBytes(bytes: Array[Byte]) : Image =
  Image.makeDeferredFromEncodedBytes(bytes)
end makeDeferredImageFromEncodedBytes
