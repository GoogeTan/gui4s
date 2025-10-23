package gui4s.desktop.kit.cats
package widgets

import io.github.humbleui.skija.Image

def image[
  Event
](
  image: Image,
): DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.image(image)
end image
