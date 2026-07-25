package gui4s.core.widget.library

import scala.reflect.Typeable
import catnip.BiMonad
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.WithContext

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
  IO[_],
  Event,
](
  transitiveStatefulWidget: [State : Typeable] => (
    name : String,
    initialState : Option[(State, Destruction)],
    eventHandler : HandlesEventF[Option[(State, Destruction)], List[(State, Destruction)], Update[Event, *]],
    body : Option[(State, Destruction)] => Widget[Either[(State, Destruction), Event]],
    destructor : Option[(State, Destruction)] => Destruction
  ) => Widget[Event],
  launchedEvent : [TaskEvent : Typeable] => (name : String, child : Widget[Event], task : IO[TaskEvent]) => Widget[Either[TaskEvent, Event]],
  doubleAllocError : [T] => () => Update[Event, T],
  emptyDesctructor : Destruction
) : ResourceWidget[Widget[Event], [T] =>> IO[(T, Destruction)]] =
  [Value : Typeable] => (name, resource) =>
    (widget : Option[Value] => Widget[Event]) =>
      transitiveStatefulWidget[Value](
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
        destructor = _.map(_._2).getOrElse(emptyDesctructor)
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
  IO[_],
  Event,
](
  transitiveStatefulWidget: [State] => (
    name : String,
    initialState : Option[State],
    eventHandler : HandlesEventF[Option[State], List[State], Update[Event, *]],
    body : Option[State] => Widget[Either[State, Event]]
  ) => Widget[Event],
  launchedEvent : [TaskEvent : Typeable] => (name : String, child : Widget[Event], task : IO[TaskEvent]) => Widget[Either[TaskEvent, Event]],
  doubleAllocError : [T] => () => Update[Event, T],
) : ResourceWidget[Widget[Event], IO] =
  [Value : Typeable] => (name, resource) =>
    (widget : Option[Value] => Widget[Event]) =>
      transitiveStatefulWidget[Value](
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
