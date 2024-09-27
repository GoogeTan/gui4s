package me.katze.gui4s.example

import draw.{SimpleDrawApi, TextStyle}

import cats.Monad
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.impl.StatefulWidgetLibraryImpl
import me.katze.gui4s.widget.library.{LabelDraw, LabelLibrary, LabelPlacement, LayoutDraw, LayoutLibrary, StatefulLibrary}
import me.katze.gui4s.widget.placeable.Placeable

class SimpleDrawApiLibrary[F[+_] : Monad, MU](api : SimpleDrawApi[F]) extends StatefulWidgetLibraryImpl[F, F[Unit], Bounds[MU]] with LabelLibrary with StatefulLibrary:
  override type LabelPlacementMeta = Unit

  override def textIsPlaceable: LabelPlacement[Placeable[Bounds[MU], Unit]] = _ => _ => ()

  override def textDraw: LabelDraw[F[Unit], Unit] = (text, _) => api.text(0, 10, text, TextStyle(18, 0, 400))
end SimpleDrawApiLibrary