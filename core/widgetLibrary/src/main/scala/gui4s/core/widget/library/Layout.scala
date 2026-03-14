package gui4s.core.widget.library

/**
 * Тип функции, описывающей установку множества виджетов в контейнер.
 * Принимает множество свободных детей и возвращает свободное множество размещенных виджетов.
 *
 * @tparam Place Эффект установки виджета
 * @tparam Collection Множества виджетов. Это может быть List, если это правило установки линейного контейнера или Id, если правило только для одного виджета.:
 * @tparam Widget Размещенный виджет
 * @tparam Meta Вспомогательные данные об результатах установки(например, координаты). TODO может, можно обобщить на произвольную комонаду
 * @todo update me
 */
trait Layout[Place[_], Collection[_], FreeWidget, IncrementalFreeWidget, PlacedWidget]:
  def place(widgets : Collection[FreeWidget]) : Place[Collection[PlacedWidget]]

  def placeIncrementally(
    widgets : Collection[IncrementalFreeWidget]
  ) : Place[Collection[PlacedWidget]]
end Layout
