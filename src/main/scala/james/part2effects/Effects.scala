package james.part2effects

import com.rockthejvm.part2effects.Effects.{MyIO, putStrLn, read}

import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  /*
  referential transparency = can replace an expression with its value
  as many times as we want without changing behavior
  */

  // example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not the same

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1)
  val changingVar_v2: Unit = () // not the same

  // side effects are inevitable for useful programs

  /*
    Effect types
    Properties:
    - type signature describes the kind of calculation that will be performed
    - type signature describes the VALUE that will be calculated
    - when side effects are needed, effect construction is separate from effect execution
   */

  // example: Option
  val anOption: Option[Int] = Option(42)
  /*
    example: Option is an effect type
    - describes a possibly absent value
    - computes a value of type A, if it exists
    - side effects are not needed
   */

  /*
  example: Future is NOT an effect type
  - describes an asynchronous computation
  - computes a value of type A, if it's successful
  - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
 */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42) // thread is scheduled when constructed instance => cannot seperate from execution

  /* example: IO
  - describes any computation that might produce side effects
  - calculates a value of type A if successful
  - side effects are required for the evaluation of () => A
    - YES, the creation of MyIO does not produce side effects on construction
  */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  /*
   pure functional programme = a big expression computing value
    - referential transparency = can replace an expression with its value without changing behaviour

  Expressions perfoming side effects are not replaceable
    - => breaking referential transparency

  Effect = data type which
    - embodies a computational concept (e.g. side effects, absence of value)
    - is referentially transparent

  Effect properties
    - it describes what kind of computation it will perform
    - the type signature describes the value it will calculate
    - it seperates effect description from effect execution
      (when externally visible side effects are produced)
  */

  /* Exercises
      1. An IO which returns the current time of the system
      2. An IO which measures the duration of a computation
      3. An IO which prints something from the console
      4. An IO which reads a line from std input
  */

  // 1
  val timeIO: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- timeIO
    _     <- computation
    finish <- timeIO
  } yield finish - start

  def demo(): Unit = {
    val myComputation = MyIO(() => Thread.sleep(1000))
    val demo: MyIO[Long] = measure(myComputation)
    println(demo.unsafeRun())
  }

  // 3
  def putStrLn(l: String): MyIO[Unit] = MyIO(() => println(l))

  // 4
  def read: MyIO[String] = MyIO(() => StdIn.readLine())

  def test(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + " " + line2)
    } yield ()

    program.unsafeRun()
  }



  def main(args: Array[String]): Unit = {
    anIO.unsafeRun()

    demo()

    test()
  }

  // run console will wait for 2 inputs
}
