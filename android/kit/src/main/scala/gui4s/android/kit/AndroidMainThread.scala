package gui4s.android.kit

import android.os.{Handler, Looper}
import scala.concurrent.ExecutionContext

object AndroidMainThread extends ExecutionContext {
  private val handler = new Handler(Looper.getMainLooper)

  override def execute(runnable: Runnable): Unit = {
    if (Looper.myLooper() == Looper.getMainLooper) {
      runnable.run()
    } else {
      handler.post(runnable)
    }
  }

  override def reportFailure(cause: Throwable): Unit = {
    cause.printStackTrace()
  }
}