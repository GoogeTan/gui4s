package gui4s.core.widget.library

import cats.*
import cats.syntax.all.given
import gui4s.core.widget.Path
import gui4s.core.widget.collectQuitCompositionReactions
import gui4s.core.widget.free.AsFreeF
import gui4s.core.widget.handle.HandlesEvent
import gui4s.core.widget.recomposition.ReactsOnRecomposition
import gui4s.core.widget.state.HasInnerStates

/**
 * Прооизводит первую установку виджета на экран. Она отличается от последующих тем, что до неё не было никакого дерева.
 * @todo Проверить, не выражается ли она через, собственно, скармливание пустого дерева как старого в обычную установку.
 * @param pathToRoot Путь до корневого виджета. Чуть рудимент. Надо будет удалить.
 * @param widget Виджет для установки
 * @param widgetReactsToRecomposition Функция, создающая реакцию виджета на рекомпозицию. См. рекомпозиция TODO ссылка на документацию по рекомпозиции
 * @param runRecomposition Запускает реакцию на рекомпозицию в общем эффекте.
 * @param runPlacement Запускает установку виджета в общем эффекте.
 * @tparam IO Общий эффект исполнения программы.
 * @tparam Widget Зазмещённый виджет.
 * @tparam Place Эффект размещения виджета.
 * @tparam Recomposition Эффект реакции на рекомпозицию.
 */
def placeForTheFirstTime[
  IO[_] : Monad,
  Widget,
  Place[_],
  Recomposition,
](
  pathToRoot : Path,
  widget : Place[Widget],
  widgetReactsToRecomposition : ReactsOnRecomposition[Widget, Recomposition],
  runRecomposition : Recomposition => IO[Unit],
  runPlacement: Place ~> IO
) : IO[Widget] =

  runPlacement(widget).flatMap(newPlacedWidget =>
    runRecomposition(widgetReactsToRecomposition(newPlacedWidget, pathToRoot, Map())).as(newPlacedWidget)
  )
end placeForTheFirstTime

/**
 * Обрабатывает внешнее событие для существующешго дерева виджетов.
 * @todo rename me
 * @param pathToRoot Путь до корневого виджета. Чуть рудимент. Надо будет удалить.
 * @param placedWidget Дерево виджетов
 * @param runRecomposition Запускает реакцию на рекомпозицию в общем эффекте.
 * @param widgetHandlesEvent Функция, обрабатывающая внешнее событие для данного типа виджетов.
 * @param widgetReactsToRecomposition Функция, создающая реакцию виджета на рекомпозицию. См. рекомпозиция TODO ссылка на документацию по рекомпозиции
 * @param widgetHasInnerState Позвращает дерево внутренних состояний дерева виджетов.
 * @param runPlacement Запускает установку виджета в общем эффекте.
 * @param event Произошедшее внешнее событие
 * @tparam IO Общий эффект исполнения программы.
 * @tparam Widget Размещённый виджет.
 * @tparam Place Эффект размещения виджета.
 * @tparam Update Эффект обновления дерева виджетов.
 * @tparam Recomposition Эффект реакции на рекомпозицию.
 * @tparam DownEvent Внешнее событие.
 * @return Обновлённое дерево виджетов.
 */
def processEvent[
  IO[_] : Monad,
  Widget,
  Place[_],
  Update[_] : Monad,
  Recomposition,
  DownEvent,
](
    pathToRoot : Path,
    runRecomposition : Recomposition => IO[Unit],
    widgetAsFree : AsFreeF[Widget, Place],
    widgetHandlesEvent : HandlesEvent[Widget, DownEvent, Update[Option[Place[Widget]]]],
    widgetReactsToRecomposition : ReactsOnRecomposition[Widget, Recomposition],
    widgetHasInnerState : HasInnerStates[Widget, Recomposition],
    runPlacement: Place ~> IO
)(
  placedWidget: Widget,
  event: DownEvent
): Update[IO[Widget]] =
  widgetHandlesEvent(placedWidget, pathToRoot, event).map:
    case Some(newWidget) =>
      for
        newPlacedWidget <- runPlacement(newWidget)
        _ <- runRecomposition(widgetReactsToRecomposition(newPlacedWidget, pathToRoot, widgetHasInnerState(placedWidget)))
        _ <- collectQuitCompositionReactions[Recomposition](
          widgetHasInnerState(placedWidget),
          widgetHasInnerState(newPlacedWidget)
        ).traverse_(runRecomposition)
      yield newPlacedWidget
    case None => 
      runPlacement(
        widgetAsFree(placedWidget)
      )
end processEvent
