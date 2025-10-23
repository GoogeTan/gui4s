package gui4s.desktop.kit.zio

import zio.*

import java.util
import java.util.concurrent.{AbstractExecutorService, LinkedBlockingQueue, TimeUnit}
import scala.concurrent.ExecutionContext

/**
 * ZIO приложение с возможностью исполнять код на главном потоке.
 */
trait MainThreadApp: 
  private lazy val runtime = Runtime.default

  def run(args: List[String]): ZIO[Any, Throwable, ExitCode]

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  final def main(args0: Array[String]): Unit =
    Unsafe.unsafe:
      unsafe =>
        val result = OneShot.make[Either[Throwable, Int]]
        val future = runtime.unsafe.runToFuture(run(args0.toList))(using summon, unsafe)
        future.onComplete { tryExitCode =>
          result.set(tryExitCode.toEither.map(_.code))
          MainThreadApp.MainExecutorService.shutdown()
        }(using MainThreadApp.mainThread)
        MainThreadApp.MainExecutorService.run()
        result.get() match
          case Right(value) => sys.exit(value)
          case Left(cause) => throw cause 
  end main  
end MainThreadApp

object MainThreadApp:
  object MainExecutorService extends AbstractExecutorService:

    private val workQueue = new LinkedBlockingQueue[Runnable]()

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    @volatile private var terminated = false

    def shutdown(): Unit =
      terminated = true

    def isShutdown: Boolean =
      terminated

    def isTerminated: Boolean =
      terminated

    @throws[InterruptedException]
    def awaitTermination(theTimeout: Long, theUnit: TimeUnit): Boolean =
      shutdown()
      terminated
    end awaitTermination

    def shutdownNow: util.List[Runnable] = new util.ArrayList[Runnable]()

    def execute(theCommand: Runnable): Unit =
      workQueue.put(theCommand)
    end execute

    /**
     * Должно быть вызвано с главного потока
     */
    @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.While"))
    def run(): Unit =
      while !terminated do
        val work = workQueue.poll(1, TimeUnit.MILLISECONDS)
        if work != null then
          work.run()
        end if
    end run
  end MainExecutorService

  /**
   * Запускает задачу на главном потоке.
   */
  val mainThread: ExecutionContext = ExecutionContext.fromExecutorService(MainExecutorService)
end MainThreadApp