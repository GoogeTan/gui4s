package gui4s.core.widget.library.decorator

import catnip.syntax.all._
import cats._

import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

/**
 * Виджет, позволяющий установить минимальные размеры для виджета.
 * @param containerWidget Обобщенный виджет контейнера
 * @param ensureMinimalSize Функция, гарантирующая минимальный размер. Она принимает измеренный виджет,
 *                          доступные границы и возвращает его с иформцией о том, как он расположе.
 * @param minSize Минимальные размеры виджета
 * @tparam Widget Размещенный виджет
 * @tparam OuterPlace Эффект установки виджета
 * @tparam InnerPlace Результат установки виджета. В частности, вероятно, это его размер.
 * @tparam Bounds Ограничения на размер виджета
 * @tparam Meta Информация о расположении виджета в контейнере
 * @return
 */
def minSizeWidget[
  Widget,
  PlacementEffect[_] : FlatMap as M,
  Situated[_],
  Bounds,
  Meta
](
 containerWidget: ContainerWidget[Widget, Id, PlacementEffect * Situated, Meta],
 ensureMinimalSize : (Situated[Widget], Bounds) => PlacementEffect[Situated[(Widget, Meta)]]
)(minSize : Bounds) : Decorator[PlacementEffect[Situated[Widget]]] =
  containerWidget(
    _,
    M.flatMap(_)(ensureMinimalSize(_, minSize))
  )
end minSizeWidget
