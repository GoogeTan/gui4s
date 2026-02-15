//> using scala "3.3"
//> using dep "io.github.humbleui:skija-shared:0.143.11"

// IMPORTANT: Uncomment the native library for your specific operating system!
//> using dep "io.github.humbleui:skija-macos-arm64:0.143.11"
// //> using dep "io.github.humbleui:skija-macos-x64:0.144.1"
// //> using dep "io.github.humbleui:skija-linux-x64:0.144.1"
// //> using dep "io.github.humbleui:skija-windows-x64:0.144.1"

import io.github.humbleui.skija._
import io.github.humbleui.skija.svg.SVGDOM
import io.github.humbleui.types.Point
import java.net.URI
import java.io.{FileOutputStream, InputStream}
import io.github.humbleui.skija.svg.SVGLength
import io.github.humbleui.skija.svg.SVGLengthUnit

// 1. Define your target size constants
val TargetWidth = 500f
val TargetHeight = 500f

val svgUrl = "https://dev.w3.org/SVG/tools/svgweb/samples/svg-files/tiger.svg"

println(s"Downloading SVG from $svgUrl...")
val in: InputStream = URI.create(svgUrl).toURL.openStream()
val bytes = in.readAllBytes()
in.close()

println("Parsing SVG...")
val data = Data.makeFromBytes(bytes)
val svgDom = new SVGDOM(data)

svgDom.setContainerSize(new Point(TargetWidth/2, TargetHeight/2))

println(s"Creating ${TargetWidth.toInt}x${TargetHeight.toInt} raster surface...")
val surface = Surface.makeRasterN32Premul(TargetWidth.toInt, TargetHeight.toInt)
val canvas = surface.getCanvas()

// 3. Clear canvas with a white background (optional, useful for transparent SVGs)
canvas.clear(0xFFFFFFFF)

println("Rendering SVG to canvas...")

svgDom.render(canvas)

// 4. Snapshot the surface and export to PNG
val image = surface.makeImageSnapshot()
val pngData = image.encodeToData(EncodedImageFormat.PNG)

val outputPath = "output_scaled.png"
val outStream = new FileOutputStream(outputPath)
outStream.write(pngData.getBytes())
outStream.close()

println(s"Success! Image saved to $outputPath")
