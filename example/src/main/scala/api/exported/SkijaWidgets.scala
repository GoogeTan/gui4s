package me.katze.gui4s.example
package api.exported

import catnip.{BiMonad, FFI}
import catnip.syntax.all.{*, given}
import cats.arrow.FunctionK
import cats.{Applicative, Apply, FlatMap, Functor, InjectK, Monad, Semigroup, ~>}
import cats.data.{EitherT, ReaderT, StateT, WriterT}
import cats.effect.{ExitCode, Sync}
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path, given}
import cats.syntax.all.*
import io.github.humbleui.skija.PathMeasure
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.example.place.RunPlacement
import me.katze.gui4s.example.update.ApplicationRequest
import me.katze.gui4s.layout.{Point3d, Sized, given}
import me.katze.gui4s.widget.library.Widget_
import scalacache.Cache
import scalacache.caffeine.CaffeineCache
import sun.jvm.hotspot.runtime.PerfMemory.end


type SkijaRecomposition[F[_]] = F[Unit]

type SkijaPlacedWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = Widget_[SkijaUpdateT[F, MeasurementUnit, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, PlaceError, Event, DownEvent]]
