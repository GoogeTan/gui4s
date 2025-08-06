package catnip

import cats.~>

/** Предоставляет интерфейс для безопасного выполнения эффектов в контексте.
 *
 * @tparam F тип эффекта, в котором будут выполняться операции (например, IO, EitherT[IO, Error, T])
 */
trait ForeighFunctionInterface[F[_]]:
  /** Оборачивает вычисление в чистый контекст.
   *
   * Используется для простых вычислений, которые не блокируют поток выполнения.
   *
   * @tparam A тип результата вычисления
   * @param trunk оборачиваемый грязный код
   * @return отложенное вычисление в контексте F
   */
  def delay[A](trunk: => A): F[A]
  
  /** Оборачивает блокирующее вычисление в чистый контекст.
   *
   * Следует использовать для операций, которые могут заблокировать текущий поток
   * (например, I/O операции или сетевые запросы).
   *
   * @tparam A тип результата операции
   * @param trunk блокирующая операция
   * @return отложенное вычисление в контексте F
   */
  def blocking[A](trunk: => A): F[A]
  
  /** Оборачивает блокирующее прерываемое вычисление в чистый контекст.
   *
   * Позволяет рантайму прервать выполнение операции, если это необходимо.
   *
   * @tparam A тип результата операции
   * @param trunk прерываемая операция
   * @return отложенное вычисление в контексте F
   */
  def interruptible[A](trunk: => A): F[A]
  
  /** Оборачивает блокирующее прерываемое множество раз вычисление в чистый контекст.
   *
   * Позволяет рантайму прервать выполнение операции множество раз, если это необходимо.
   *
   * @tparam A тип результата операции
   * @param trunk многократно прерываемая операция
   * @return отложенное вычисление в контексте F
   */
  def interruptibleMany[A](trunk: => A): F[A]

  /** Синтаксический сахар для метода delay.
   * Оборачивает вычисление в чистый контекст.
   *
   * Используется для простых вычислений, которые не блокируют поток выполнения.
   *
   * @tparam A тип результата вычисления
   * @param trunk оборачиваемый грязный код
   * @return отложенное вычисление в контексте F
   */
  final def apply[A](trunk: => A): F[A] = delay(trunk)
  
  final def mapK[G[_]](f : F ~> G) : ForeighFunctionInterface[G] =
    new ForeighFunctionInterface[G]:
      override def delay[A](trunk: => A): G[A] = f(ForeighFunctionInterface.this.delay(trunk))

      override def blocking[A](trunk: => A): G[A] = f(ForeighFunctionInterface.this.blocking(trunk))

      override def interruptible[A](trunk: => A): G[A] = f(ForeighFunctionInterface.this.interruptible(trunk))

      override def interruptibleMany[A](trunk: => A): G[A] = f(ForeighFunctionInterface.this.interruptibleMany(trunk))
    end new
  end mapK
end ForeighFunctionInterface