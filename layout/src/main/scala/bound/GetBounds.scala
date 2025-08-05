package me.katze.gui4s.layout
package bound

import bound.Bounds
import catnip.Get

type GetBounds[F[_], MeasurementUnit] = Get[F, Bounds[MeasurementUnit]]