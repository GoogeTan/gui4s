package gui4s.desktop.kit.zio
package widgets

import effects.given
import io.github.humbleui.skija.Image
import zio.*
import zio.interop.catz.*

def image[
  Event
](
  image: Image,
): DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.image(image)
end image
