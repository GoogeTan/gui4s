package gui4s.desktop.kit.cats
package widgets

import effects.given

import io.github.humbleui.skija.Image

def image[
  Event
](
  image: Image,
): DesktopWidget[Event] =
  gui4s.desktop.kit.widgets.image(image)
end image
