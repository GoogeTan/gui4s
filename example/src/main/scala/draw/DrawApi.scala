package me.katze.gui4s.example
package draw

import me.katze.gui4s.layout.bound.Bounds

final case class DrawApi[F[_], MeasurementUnit, Draw](
                                                        windowBounds : F[Bounds[MeasurementUnit]],
                                                        graphics : SimpleDrawApi[MeasurementUnit, Draw]
                                                      )

