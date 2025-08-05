package me.katze.gui4s.layout
package bound

import bound.Bounds
import catnip.Set

type SetBounds[F[_], MeasurementUnit] = Set[F, Bounds[MeasurementUnit]]
