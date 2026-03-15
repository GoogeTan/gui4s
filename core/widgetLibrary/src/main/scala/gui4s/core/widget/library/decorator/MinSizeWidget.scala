package gui4s.core.widget.library.decorator

import catnip.syntax.all.given
import cats._
import cats.syntax.all._

import gui4s.core.layout.ContainerStrategy
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

/**
 * Виджет, позволяющий установить минимальные размеры для виджета.
 * @param containerWidget Обобщенный виджет контейнера
 * @param ensureMinimalSize Функция, гарантирующая минимальный размер. Она принимает измеренный виджет,
 *                          доступные границы и возвращает его с иформцией о том, как он расположе.
 * @param minSize Минимальные размеры виджета
 * @tparam PlacedWidget Размещенный виджет
 * @tparam OuterPlace Эффект установки виджета
 * @tparam InnerPlace Результат установки виджета. В частности, вероятно, это его размер.
 * @tparam Bounds Ограничения на размер виджета
 * @tparam Meta Информация о расположении виджета в контейнере
 * @return
 */
def minSizeWidget[
  Widget,
  PlacedWidget,
  MeasuredWidget,
  PlacementEffect[_] : FlatMap as M,
  Size,
  Bounds,
  Point,
  PositionedWidget
](
  containerWidget : ContainerWidget[
    Widget,
    Widget,
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[MeasuredWidget],
      Size,
      Bounds,
      Id,
      PositionedWidget
    ]
  ],
  getBounds : PlacementEffect[Bounds],
  ensureMinimalSize : OneElementPlacementStrategy[PlacementEffect, Size, Size, Bounds, Point],
  makeMeta : (MeasuredWidget, Bounds, Point) => PositionedWidget,
  itemSize : MeasuredWidget => Size
)(minSize : Bounds) : Decorator[Widget] =
  containerWidget(
    _,
    ContainerStrategy.combine(
      measurementStrategy = child => getBounds.flatMap(bounds => child.map(childPlaced => (childPlaced, bounds))),
      placementStrategy = ensureMinimalSize,
      someMap = {
        case ((widget, bounds), point) => makeMeta(widget, bounds, point)
      },
      sizeOfItem = (item, _) => itemSize(item)
    )
  )
end minSizeWidget
