package gui4s.core.widget.library

/**
 * Декторатор, позволяющий применить функцию к событиям, кидаемым виджетом.
 * @tparam Widget Виджета, параметризован типом события
 * @todo Обобщить с возможностью фильтрации
 */
type MapEvent[Widget[_]] = [A, B] => (A => B) => Widget[A] => Widget[B]
