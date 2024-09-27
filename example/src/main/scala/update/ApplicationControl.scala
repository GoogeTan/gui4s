package me.katze.gui4s.example
package update

import cats.effect.ExitCode

/**
 * Позволяет управлять потоками приложения.
 * @param join ожидает закрытия приложения(успешного или с кодом ошибки).
 * @param close закрывает приложение.
 * @param pushEvent отправляет внешнее событие корневому виджету.
 * @tparam DownEvent тип событий дерева виджетов. 
 */
final case class ApplicationControl[+F[_], -DownEvent](close : F[Unit], join : F[ExitCode], pushEvent : DownEvent => F[Unit])
