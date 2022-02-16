package james.part2effects

import cats.effect.{IO, IOApp}
import james.utils.*
import cats.syntax.apply.*
import cats.{Id, Parallel}
import cats.data.Kleisli
import cats.effect.IO.Par

object IOParallelism extends IOApp.Simple {
  // IOs are usually sequential

  val ioA = IO(println(s"${Thread.currentThread().getName}: A"))
  val ioB = IO(println(s"${Thread.currentThread().getName}: B"))

  // if I compose, will be composed on the same thread
  val composedIO = for {
    a <- ioA
    b <- ioB
  } yield s"$a AND $b"

  val mol: IO[Int] = IO.delay(42)
  val fL: IO[String] = IO.delay("scala")
  val goalInLife = (mol, fL).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to paralell IO
  val gil = (mol.debug, fL.debug).mapN((num, string) => s"my goal in life is $num and $string")
  val par101: IO.Par[Int] = Parallel[IO].parallel(mol.debug)
  val par102: IO.Par[String] = Parallel[IO].parallel(fL.debug)
  import cats.effect.implicits.*
  val gilParallel: Par[String] = (par101, par102).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val gil_v2: IO[String] = Parallel[IO].sequential(gilParallel)

  // shorthand
  import cats.syntax.parallel.*
  val goalInLife_v3: IO[String] = (mol.debug, fL.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // what happens if one of our IOs fails
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("fail"))
  // compose success and failure
  val parallelWithFailure = (mol.debug, aFailure.debug).parMapN(_ + _) // can handle like in IOErrorHandling
  // compose a failure with another failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  // first effect to fail gives result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] =
//    composedIO.map(println)
//    goalInLife.map(println) // same thread
//  gil_v2.map(println) // separate threads
//  gil_v2.debug.void // 3 different threads
//  goalInLife_v3.debug.void
    twoFailuresDelayed.debug.void
}
