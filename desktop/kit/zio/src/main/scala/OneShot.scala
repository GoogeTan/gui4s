package gui4s.desktop.kit.zio

import zio.stacktracer.TracingImplicits.disableAutoTrace

import java.util.concurrent.locks.{Condition, ReentrantLock}

/**
 * A variable that can be set a single time. The synchronous, effectful
 * equivalent of `Promise`.
 */
@SuppressWarnings(Array("org.wartremover.warts.All"))
final class OneShot[A] private () extends ReentrantLock(false) {
  @volatile private var value                 = null.asInstanceOf[A & AnyRef]
  private final val isSetCondition: Condition = this.newCondition()

  /**
   * Sets the variable to the value. The behavior of this function is undefined
   * if the variable has already been set.
   */
  def set(v: A): Unit = {
    if (v == null) throw new Error("Defect: OneShot variable cannot be set to null value")

    this.lock()

    try {
      if (value ne null) throw new Error("Defect: OneShot variable being set twice")

      value = v.asInstanceOf[A & AnyRef]

      this.isSetCondition.signalAll()
    } finally {
      this.unlock()
    }
  }

  /**
   * Determines if the variable has been set.
   */
  def isSet: Boolean = value ne null

  /**
   * Retrieves the value of the variable, blocking if necessary.
   *
   * @param timeout
   *   The maximum amount of time the thread will be blocked, in milliseconds.
   * @throws Error
   *   if the timeout is reached without the value being set.
   */
  def get(timeout: Long): A =
    if (value ne null) value
    else {
      this.lock()

      try {
        if (value eq null) this.isSetCondition.await(timeout, java.util.concurrent.TimeUnit.MILLISECONDS)
      } finally {
        this.unlock()
      }

      if (value eq null) throw new OneShot.TimeoutException

      value
    }

  /**
   * Retrieves the value of the variable, blocking if necessary.
   *
   * This will block until the value is set or the thread is interrupted.
   */
  def get(): A =
    if (value ne null) value
    else {
      this.lock()

      try {
        while (value eq null) this.isSetCondition.await()
      } finally {
        this.unlock()
      }

      value
    }

}

@SuppressWarnings(Array("org.wartremover.warts.All"))
object OneShot: 

  private final val nanosPerMilli = 1000000L

  /**
   * Makes a new (unset) variable.
   */
  def make[A]: OneShot[A] = new OneShot()

  final class TimeoutException extends Error("Timed out waiting for variable to be set")
end OneShot

