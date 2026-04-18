package gui4s.desktop.skija.svg

import catnip.resource.SyncResource
import io.github.humbleui.skija.Data
import io.github.humbleui.skija.svg.SVGDOM

def makeSvgFromData[Resource[_] : SyncResource as S](data : Data) : Resource[SVGDOM] =
  S.fromAutoCloseable(SVGDOM(data))
end makeSvgFromData
