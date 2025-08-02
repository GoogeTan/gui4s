package me.katze.gui4s.geometry

import scala.annotation.tailrec
import scala.compiletime.ops.int.*

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Throw"))
enum PointNd[MeasurementUnit, N <: Int]: 
  case OneD(coordinate : MeasurementUnit) extends PointNd[MeasurementUnit, 1]
  case ND(coordinate : MeasurementUnit, tail : PointNd[MeasurementUnit, N - 1]) extends PointNd[MeasurementUnit, N]

  @tailrec
  final def apply(index : Int)(using p : index.type < N) : MeasurementUnit =
    (index, this) match
      case (0, OneD(coordinate)) => coordinate
      case (0, ND(coordinate, _)) => coordinate
      case (index, ND(coordinate, tail)) =>
        val prev = index - 1
        tail(prev)(using p.asInstanceOf[prev.type < N - 1])
      case (index, OneD(_)) => throw Exception("It is absolutely impossible to get here.")