package james.part2effects

import cats.effect.IO

object IOErrorHandling {

  // IO: pure, delay, defer
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A Failure"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("run time error"))
    case _ => IO.delay(println("Something else went wrong"))
  }

  // transform into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")
  // redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO.delay(println(s"FAIL: $ex")), value => IO.delay(println(s"SUCCESS: $value")))

  /**
   * exercises
   */

  // 1 - construct potentially failed IOs from standard data types (Option, Either)

  def optionToIO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match
      case Some(a) => IO(a)
      case None => IO.raiseError(ifEmpty)

  def eitherToIO[A](either: Either[Throwable, A]): IO[A] =
    either match
      case Left(ex) => IO.raiseError(ex)
      case Right(a) => IO(a)

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity) // identity == (a => a)


  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = {
    io.redeemWith(handler, IO.pure)
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    dealWithIt.unsafeRunSync()
    println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()
  }
}
