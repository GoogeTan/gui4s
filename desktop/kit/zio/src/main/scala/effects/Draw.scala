package gui4s.desktop.kit.zio
package effects

import gui4s.desktop.skija.SkijaDraw
import gui4s.desktop.skija
import catnip.effect.* 
import zio.* 
import zio.interop.catz.*
import io.github.humbleui.skija.Image
import cats.effect.Sync

type Draw = SkijaDraw[Task]

object Draw:
  val ffi = SyncForeignFunctionInterface[Task](using summon[Sync[Task]])
  def drawAt(whatToDraw : Draw, x : Float, y : Float) : Draw =
    skija.drawAt(ffi, whatToDraw, x, y)
  end drawAt

  def drawImage(image : Image) : Draw =
    skija.drawImage(ffi, image)
  end drawImage

  def drawText(text : skija.SkijaPlacedText) : Draw =
    skija.drawText(ffi, text)
  end drawText

  def drawClipped(path: Clip, original: Draw): Draw =
    skija.clipToPath(ffi, path, original)
  end drawClipped
end Draw
