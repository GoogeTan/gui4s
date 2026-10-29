package gui4s.desktop.skija.svg

import catnip.resource.SyncResource
import io.github.humbleui.skija.Data
import io.github.humbleui.skija.svg.SVGDOM

def makeSvgFromData[Resource[_] : SyncResource as S](data : Data) : Resource[SVGDOM] =
  S.fromAutoCloseable(SVGDOM(data))
end makeSvgFromData

type SVGDOM = io.github.humbleui.skija.svg.SVGDOM
type SVGLength = io.github.humbleui.skija.svg.SVGLength
type SVGLengthContext = io.github.humbleui.skija.svg.SVGLengthContext
type SVGLengthUnit = io.github.humbleui.skija.svg.SVGLengthUnit

object SVGLengthUnit:
  val PX : SVGLengthUnit = io.github.humbleui.skija.svg.SVGLengthUnit.PX
end SVGLengthUnit
