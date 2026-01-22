package gui4s.core.widget.library

import gui4s.core.widget.handle.Layout

/**
 * Тип виджета контейнера.
 * Он позволяет делать любые операции по размещению виджетов в пространстве.
 *
 * @example Частными случаями являются строка, столбец, стопка, коробка. Для первых трех примеров тип множества - List,
 * для коробки - Id.
 *
 * @param children Множество дочерних виджетов контейнера.
 * @param layout Функция, задающая расстановку дочерних виджетов. См. [[Layout]].
 *
 * @tparam PlacedWidget Размещенный виджет.
 * @tparam Container Множество виджетов.
 * @tparam Place Эффект установки виджета.
 * @tparam Meta Вспомогательные данные об результате установки виджета. TODO примеры
 */
type ContainerWidget[PlacedWidget, Container[_], Place[_], Meta] =
  (children : Container[Place[PlacedWidget]], layout : Layout[Place, Container, PlacedWidget, Meta]) => Place[PlacedWidget]
