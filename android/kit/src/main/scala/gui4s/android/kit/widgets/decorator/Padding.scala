package gui4s.android.kit.widgets.decorator

import cats.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.core.geometry.{InfinityOr, Point3d, Rect}
import gui4s.core.layout.{Measured, Sized}
import gui4s.core.widget.library.decorator.{PaddingWidget, Paddings}

import scala.annotation.targetName

/**
 * Одноместный контейнер, добавляющий отступы вокруг виджета.
 */
def paddingWidget[Event] : PaddingWidget[AndroidWidget[Event], Paddings[InfinityOr[Float]]] =
  lazy val error = new Throwable("Infinite padding can not be placed in an infinite container.")
  paddings => widget =>
    gui4s.desktop.widget.library.decorator.paddingWidget(
      container = oneElementContainerWidget[Event],
      infinitePaddingInInfiniteContainer = error,
      boundsWithPaddings =
        PlacementEffect.withBoundsK(bounds =>
          bounds.cut(
            width = paddings.left.value.getOrElse(0f) + paddings.right.value.getOrElse(0f) ,
            height = paddings.top.value.getOrElse(0f)  + paddings.bottom.value.getOrElse(0f) ,
            cut = _.minus(_)
          )
        ),
      makeMeta = (measured, point) => Measured((measured.value, new Point3d(point, 0)), measured.size, measured.bounds),
      sizeOfItem  = _.size,
      widget = widget,
      paddings = paddings
    )
end paddingWidget

extension[Event](widget: AndroidWidget[Event])
  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  @targetName("simplePadding")
  def padding(paddings: Paddings[Float]): AndroidWidget[Event] =
    paddingWidget[Event](paddings.map(new InfinityOr(_)))(widget)
  end padding

  /**
   * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
   */
  def padding(paddings: Paddings[InfinityOr[Float]]): AndroidWidget[Event] =
    paddingWidget(paddings)(widget)
  end padding
end extension
