package gui4s.core.widget.library

import catnip.BiMonad
import cats.Functor
import gui4s.core.widget.library.{TransitiveStatefulWidget, WithContext}

import scala.reflect.Typeable

/**
 * Тип виджета, позволяющего безопасно создавать ресурсы.
 * @tparam Resource Эффекта ресурса
 * @tparam T Ресурс
 * @param name Имя состояния виджета
 * @param resource Эффект, создающий ресурс
 * @returns Виджет с контекстом ресурса(пока ресурс не закончил инициализацию, будет передано None). С момента, как передано Some, всегда будет передано Some
 */
type ResourceWidget[Widget, Resource[_]] = [T : Typeable] => (name : String, resource : Resource[T]) => WithContext[Widget, Option[T]]

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
given destructableIsTypeable[T: Typeable, IO: Typeable]: Typeable[(T, IO)] = x => x match {
  case (a: T, io: IO) =>
    Some[(T, IO)]((a, io)).map(_.asInstanceOf[x.type & (T, IO)])
  case _ => None
}

/**
 * Реализация ResourceWidget, позволяющая безопасно создавать ресурсы с деструктором.
 * В более конктретных модулях будет эффект Resource, а не его частный случай.
 *
 * @param transitiveStatefulWidget
 * @param launchedEvent
 * @param doubleAllocError
 * @param emptyDesctructor
 * @tparam Destruction Эффект, в котором исполняются деструкторы данного transitiveStatefulWidget
 * @tparam IO Эффект инициализации ресурса
 * @tparam Event
 * @todo Заменить слишком общий интерфейс TransitiveStatefulWidget на конкретный тип функции.
 * @todo Добавить докумнетацию ко всем параметрам
 * @return
 */
def destructibleResourceWidget[
  Widget[_],
  Update[_, _] : BiMonad as updateBiMonad,
  Place[_],
  Destruction : Typeable,
  IO[_] : Functor,
  Event,
](
  transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Update, [State] =>> State => Destruction, Nothing],
  launchedEvent : [TaskEvent : Typeable] => (name : String, child : Widget[Event], task : IO[TaskEvent]) => Widget[Either[TaskEvent, Event]],
  doubleAllocError : [T] => () => Update[Event, T],
  emptyDesctructor : Destruction
) : ResourceWidget[Widget[Event], [T] =>> IO[(T, Destruction)]] =
  [Value : Typeable] => (name, resource) =>
    (widget : Option[Value] => Widget[Event]) =>
      transitiveStatefulWidget[
        Option[(Value, Destruction)],
        Event,
        (Value, Destruction)
      ](
        name = name,
        initialState = None,
        eventHandler = {
          case (None, event :: Nil) =>
            updateBiMonad[Event]().pure(Some(event))
          case (state, Nil) => updateBiMonad[Event]().pure(state)
          case (_, _) => doubleAllocError()
        },
        body = state =>
          launchedEvent[(Value, Destruction)](
            "effect_launcher",
            widget(state.map(_._1)),
            resource
          ),
        destructor = {
          case Some((_, destructor)) => destructor
          case None => emptyDesctructor
        }
      )
end destructibleResourceWidget

/**
 * Виджет, позволяющий единоразово иницилизировать значение с эффектом.
 * @param launchedEvent
 * @param doubleAllocError
 * @tparam IO Эффект инициализации ресурса
 * @todo Заменить слишком общий интерфейс TransitiveStatefulWidget на конкретный тип функции.
 * @todo Добавить докумнетацию ко всем параметрам
 * @return
 */
def initializeResourceWidget[
  Widget[_],
  Update[_, _] : BiMonad as updateBiMonad,
  Place[_],
  IO[_] : Functor,
  Event,
](
  transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Update, Nothing, Nothing],
  launchedEvent : [TaskEvent : Typeable] => (name : String, child : Widget[Event], task : IO[TaskEvent]) => Widget[Either[TaskEvent, Event]],
  doubleAllocError : [T] => () => Update[Event, T],
) : ResourceWidget[Widget[Event], IO] =
  [Value : Typeable] => (name, resource) =>
    (widget : Option[Value] => Widget[Event]) =>
      transitiveStatefulWidget[
        Option[Value],
        Event,
        Value
      ](
        name = name,
        initialState = None,
        eventHandler = {
          case (None, event :: Nil) =>
            updateBiMonad[Event]().pure(Some(event))
          case (_, _) => doubleAllocError()
        },
        body = state =>
          launchedEvent[Value](
            "effect_launcher",
            widget(state),
            resource
          ),
      )
end initializeResourceWidget
