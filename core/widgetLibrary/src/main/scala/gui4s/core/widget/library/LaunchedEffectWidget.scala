package gui4s.core.widget.library

/**
 * Тип виджета, запускающего задачу(эффект) каждый раз, когда изменяется параметр(ключ).
 *
 * @param name Имя состояния виджета
 * @param child Оригинальный виджет
 * @param key Нынешний ключ
 * @todo Использовать тут тип декоратора
 * @todo Добавить гарантию, что старая задача закрывается при запуске новой, если ещё не завершилась сама
 *
 * @tparam Widget Свободый виджет
 * @tparam Key Ключа
 * @tparam Task Задача
 */
type LaunchedEffectWidget[Widget, Key, Task] = (name : String, child : Widget, key : Key, task : Task) => Widget
