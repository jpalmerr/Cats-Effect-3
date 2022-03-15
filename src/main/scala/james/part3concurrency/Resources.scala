package james.part3concurrency

import cats.effect.{IO, IOApp, Resource}
import james.utils.*

import java.io.{File, FileReader}
import java.util.Scanner
import concurrent.duration.*

object Resources extends IOApp.Simple{
  // use case: manage a connection life cycle
class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").debug
    def close: IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("Hi James").open *> IO.sleep((Int.MaxValue).seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  // Open but never Closed. problem: leaking resources

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("Hi James"))
    fib  <- (conn.open *> IO.sleep((Int.MaxValue).seconds)).onCancel(conn.close.void).start // onCancel will free up resources
    _    <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  /**
   * [io-compute-2] opening connection to Hi James
   * [io-compute-4] closing connection to Hi James
   *
   * tediuous and hard to understand as we get more complex => the bracket pattern
   */

  /*
    the bracket pattern someIO.bracket(use)(release)
    like a functional try catch
   */
  val bracketFetchUrl = IO(new Connection("james.com"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /**
   * Exercise
   * - open a scanner
   * - read the file line by line, every 100 milis
   * - close scanner if successful, close scanner if cancelled
   */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner((new FileReader(new File(path)))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] = {
    IO("opening file") >>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug >> IO(scanner.close())
      }
  }

  /**
   * Resources
   */

  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      // acquire a connection based on the file
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open.debug >> IO.never
      }(conn => conn.close.debug.void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))
    // nesting resources are tedious

    // Resource.make(acquire)(release)
    val connectionResource = Resource.make(IO(new Connection("james")))(conn => conn.close.void) // usage can happen later

    val resourceFetchUrl = for {
      fib <- connectionResource.use(conn => conn.open >> IO.never)
      _ <- IO.sleep(1.second) >> fib.cancel
    } yield ()

    // resources are equivalent to brackets

    val simpleResource = IO("some resource")
    val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
    val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

    val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
    val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)




  override def run: IO[Unit] = {
//    bracketProgram.void
    bracketReadFile("src/main/scala/james/part3concurrency/Resources.scala")
//    resourceFetchUrl.void // doesn't like the type of fib for some reason
  }
}
