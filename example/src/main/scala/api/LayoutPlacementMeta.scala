package me.katze.gui4s.example.api

import me.katze.gui4s.geometry.Point3d
import me.katze.gui4s.layout.Placed

final case class LayoutPlacementMeta[+MeasurementUnit](x: MeasurementUnit, y: MeasurementUnit, z : MeasurementUnit):
  def this(placed : Placed[MeasurementUnit, ?]) =
    this(placed.x, placed.y, placed.z)
    
  def point : Point3d[MeasurementUnit] = Point3d(x, y, z)

given orderByZ[MeasurementUnit : Ordering as O] : Ordering[LayoutPlacementMeta[MeasurementUnit]] = (a, b) => O.compare(a.z, b.z)