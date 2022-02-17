package james.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Succeeded, Errored, Canceled}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome}

import scala.concurrent.duration.*

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLanguage = IO.pure("Scala")

  import james.utils.*
  // evaluated on same thread
  def simpleIOComposition() = for {
    _ <- meaningOfLife.debug
    _ <- favLanguage.debug
  } yield ()

  // introduce the fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  val aFiber: IO[FiberIO[Int]] = meaningOfLife.debug.start // allocation of a fiber is effectful => wrapped in an effect type itself

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLanguage.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
    IO[ResultType of fib.join]
    fib.join = Outcome[IO, ErrorType, IO[A]]

    possible outcomes:
      - success with an IO
      - failure with an exception
      - cancelled
  */

  val someIOOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
      case Succeeded(effect) => effect
      case Errored(e) => IO(0)
      case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    // // onCancel is a "finalizer", allowing you to free up resources in case you get canceled
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled!").debug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a seperate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug
      _ <- fib.cancel
      result <- fib.join
    }  yield result
  }

  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   */

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult: IO[Outcome[IO, Throwable, A]] =
      for {
        fib <- io.debug.start
        res <- fib.join
      } yield res
    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Cancelled"))
    }
  }

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  /**
   *  2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - if one (or both) canceled, raise a RuntimeException
   */

  def tupleIOs[A, B](ioA: IO[A], ioB: IO[B]): IO[(A, B)] = {
    val result: IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] = for {
      fibA <- ioA.start
      fibB <- ioB.start
      resultA <- fibA.join
      resultB <- fibB.join
    } yield (resultA, resultB)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        for {
          a <- fa
          b <- fb
        } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation cancelled"))
    }
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug

    tupleIOs(firstIO, secondIO).debug.void
  }

  /**
   *  3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is canceled
   *    - the method returns an IO[A] which contains
   *    - the original value if the computation is successful before the timeout signal
   *    - the exception if the computation is failed before the timeout signal
   *    - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- IO.sleep(duration) >> fib.cancel
      res <- fib.join
    } yield res

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  /* if timeout is very long should we wait for timeout to finish
     can start cancel fiber on another thread
  */
  def timeoutHandleOnAnotherThread[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful as fibers can leak => is timeout effective? (resources they handle that can leak)
      res <- fib.join
    } yield res

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testEx3(duration: FiniteDuration) = {
    val aComputation = IO("starting").debug >> IO.sleep(duration) >> IO("done!").debug >> IO(42)
    timeout(aComputation, 2.seconds).debug.void
  }

  def testEx3pt2(duration: FiniteDuration) = {
    val aComputation = IO("starting").debug >> IO.sleep(duration) >> IO("done!").debug >> IO(42)
    timeoutHandleOnAnotherThread(aComputation, 2.seconds).debug.void
  }

  override def run: IO[Unit] = {
    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
      .debug.void

    throwOnAnotherThread().debug.void // Errored(java.lang.RuntimeException: no number for you)

    testCancel().debug.void

    testEx1()

    testEx2()
    testEx3(5.second)
    testEx3(1.second)
    testEx3pt2(1.second) // notice done! and 42 happen at same time
  }
}
