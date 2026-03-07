package gui4s.core.widget.library

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
 * @tparam Collection Множество виджетов.
 * @tparam Place Эффект установки виджета.
 * @tparam Meta Вспомогательные данные об результате установки виджета. TODO примеры
 * @todo Rewrite me
 */
type ContainerWidget[Widget, Children, Placement] =
  (
    children : Children,
    placementStrategy : Placement
  ) => Widget
