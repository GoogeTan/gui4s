package gui4s.desktop.kit.zio

import java.util
import java.util.concurrent.{AbstractExecutorService, LinkedBlockingQueue, TimeUnit}
import zio.*

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.concurrent.JavaConversions.asExecutionContext

/**
 * The entry-point for a ZIO application that must run certain effects on the main thread.
 */
trait MainThreadApp: 
  private lazy val runtime = Runtime.default

  def run(args: List[String]): ZIO[Any, Throwable, ExitCode]

  /**
   * The Scala main function, intended to be called only by the Scala runtime.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  final def main(args0: Array[String]): Unit =
    Unsafe.unsafe:
      unsafe =>
        val result = OneShot.make[Either[Throwable, Int]]
        val future = runtime.unsafe.runToFuture(run(args0.toList))(using summon, unsafe)
        future.onComplete {
          case Failure(exception) =>
            result.set(Left(exception))
            MainThreadApp.MainExecutorService.shutdown()
          case Success(value) => 
            result.set(Right(value.code))
            MainThreadApp.MainExecutorService.shutdown()
        }(using MainThreadApp.MainExecutorService)
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
     * Must be called synchronously from the main thread.
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
   * The main thread is exposed here as an Executor to be used via [[ZIO.lock()]].
   */
  val mainThread: ExecutionContext = ExecutionContext.fromExecutorService(MainExecutorService)
end MainThreadApp